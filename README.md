# ffunc

**ffunc** is an R package containing fast utility functions, implemented in C.

Functions:
* `fsubstr` - Fast substring
* `fstr_sub` - Fast substring which allows negative start and stop positions

## Installation

Run the following commands in R. If necessary, install the devtools package first.
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
> string <- "abcdefgh"

> fsubstr(string, 2, 5)
[1] "bcde"

> fsubstr(string, 2, 5) <- "_"
> string
[1] "a_cdefgh"

> fsubstr(string, 2, 5) <- "KLMNOP"
> string
[1] "aKLMNfgh"
```

#### Performance

`fsubstr` is **35%** faster than `substr` on ASCII strings, **75%** faster for UTF-8 Unicode and **4.5x** the speed of `substr` on Windows-932 native encoding.


<details>
      <summary>Benchmark results</summary>

```R
> # ASCII:
> microbenchmark::microbenchmark(
+   substr( test_strings, start, stop),
+   fsubstr(test_strings, start, stop)
+ )
Unit: milliseconds
                               expr    min      lq     mean median      uq     max neval
  substr(test_strings, start, stop) 3.7688 3.82560 4.012606 3.8862 3.94030 12.4761   100
 fsubstr(test_strings, start, stop) 2.6653 2.80215 3.071273 2.8713 3.10785 11.1838   100

> # Unicode:
> microbenchmark::microbenchmark(
+   substr( unicode_strings, start, stop),
+   fsubstr(unicode_strings, start, stop)
+ )
Unit: milliseconds
                                  expr    min      lq     mean  median      uq     max neval
  substr(unicode_strings, start, stop) 9.1654 9.23155 9.583475 9.38705 9.55365 17.2722   100
 fsubstr(unicode_strings, start, stop) 4.9629 5.10895 5.552599 5.30215 5.99965  7.0865   100

> # Native multibyte (Windows-932 Japanese):
> microbenchmark::microbenchmark(
+   substr( test_strings, start, stop),
+   fsubstr(test_strings, start, stop)
+ )
Unit: milliseconds
                               expr     min       lq      mean    median        uq      max neval
  substr(test_strings, start, stop) 99.2071 100.4725 102.19818 101.89140 102.51550 149.4295   100
 fsubstr(test_strings, start, stop) 22.2096  22.5142  24.08002  22.75465  23.29965  40.4882   100
```
_Tested on Intel(R) Core(TM) i5-8400 CPU @ 2.80GHz (6 Cores), 8 GB RAM, R version 4.0.2_
</details>

Replacement of substrings using `fsbustr()<-` is **50%** faster than `substr()<-` for Unicode and **13%** faster for native encodings, but is **-5%** slower in the case of purely ASCII strings.

<details>
      <summary>Benchmark results</summary>

```R
> # ASCII:
> microbenchmark::microbenchmark(
+   substr( test_strings,   start, stop) <- rep_string,
+   fsubstr(test_strings_2, start, stop) <- rep_string
+ )
Unit: milliseconds
                                               expr     min      lq     mean   median       uq     max neval
    substr(test_strings, start, stop) <- rep_string  9.6011  9.8018 10.14384  9.98060 10.20840 17.2230   100
 fsubstr(test_strings_2, start, stop) <- rep_string 10.1201 10.3400 10.75959 10.54735 10.76565 17.6955   100
 
 > # Unicode:
 > microbenchmark::microbenchmark(
+   substr( unicode_strings,   start, stop) <- rep_unicode,
+   fsubstr(unicode_strings_2, start, stop) <- rep_unicode
+ )
Unit: milliseconds
                                                   expr     min       lq     mean   median       uq     max neval
    substr(unicode_strings, start, stop) <- rep_unicode 37.2295 37.51450 38.07830 37.79465 38.45315 46.9437   100
 fsubstr(unicode_strings_2, start, stop) <- rep_unicode 23.5424 25.07725 25.51652 25.29865 25.63595 34.6300   100
 
 > # Native multibyte (Windows-932 Japanese):
> microbenchmark::microbenchmark(
+   substr( test_strings,   start, stop) <- rep_string,
+   fsubstr(test_strings_2, start, stop) <- rep_string
+ )
Unit: milliseconds
                                               expr      min       lq     mean   median       uq      max neval
    substr(test_strings, start, stop) <- rep_string 122.5946 123.4407 124.9449 123.9614 125.9677 134.5395   100
 fsubstr(test_strings_2, start, stop) <- rep_string 107.9184 109.5022 110.4082 109.9912 110.8768 116.9503   100
```
_Tested on Intel(R) Core(TM) i5-8400 CPU @ 2.80GHz (6 Cores), 8 GB RAM, R version 4.0.2_
</details>

#### How `fsubstr()` works

1. The string vector is split into batches, each one shorter than a set limit (`FAST_ARRAY_MEM_LIMIT`) to avoid slow memory access (e.g. due to cache misses)<br />
![fsubstr routine](docs/fsubstr.png "fsubstr routine").
2. For each batch:
    1. each string in a batch is processed using the internal function `substrSingleElt()`, which can run on multiple threads in parallel. The results are stored in the temporary array `substrings`;
    2. the substrings are then written into the `output` string vector (writing strings must be done in series because writing to R's string hash table is not thread-safe). The majority of `fsubstr()`'s execution time is spent here.

`fsubstr()<-` does not use parallel processing because a new string is produced for each string in the input vector - storing these in a temporary vector can take a considerable amount of memory if the input strings are long enough. By contrast, in `fsubstr()`, only the start and end points and the encoding of each substring need to be stored, so the memory usage is limited.


#### UTF-8

When `fsubstr()<-` is used on a pair of strings where one string is in UTF-8 encoding and the other is in native encoding then the native string is translated to UTf-0 as part of the substring replacement. This differs from the behaviour of `substr()<-`, which translates the UTF-8 to native, resulting in '<U+####>' for any UTF-8 characters that are not represented in the native encoding (see https://cran.r-project.org/doc/manuals/R-exts.html#Character-encoding-issues).


### `fstr_sub(x, start, stop)`

Another fast substring function, in which negative start and stop arguments may be used to count backwards from the end of the input strings.

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

#### Performance

`fstr_sub` is **90%** faster than `str_sub` on ASCII strings, **75%** faster on UTF-8 Unicode and **over 2x** the speed of `str_sub` on Windows-932 native encoding.

<details>
      <summary>Benchmark results</summary>

```R
> substrlen <- function(strings, start, stop, str_len) {
+   substr(strings, ifelse(start < 0, pmax(start + str_len + 1, 1), start), 
+                   ifelse(stop  < 0, pmax(stop  + str_len + 1, 0), stop))
+ }

> # ASCII:
> microbenchmark::microbenchmark(
+   str_sub(  test_strings, start, stop),
+   substrlen(test_strings, start, stop, len),
+   fstr_sub( test_strings, start, stop)
+ )
Unit: milliseconds
                                      expr      min       lq     mean   median       uq     max neval
        str_sub(test_strings, start, stop) 5.402701 5.555951 5.848328 5.661601 5.869951 14.8824   100
 substrlen(test_strings, start, stop, len) 3.892301 4.000451 4.220415 4.083602 4.207301 11.9097   100
       fstr_sub(test_strings, start, stop) 2.680801 2.865401 3.255502 2.919801 3.318201 11.9894   100

> # Unicode:
> microbenchmark::microbenchmark(
+   str_sub(  unicode_strings, start, stop),
+   substrlen(unicode_strings, start, stop, len),
+   fstr_sub( unicode_strings, start, stop)
+ )
Unit: milliseconds
                                         expr     min       lq      mean   median       uq     max neval
        str_sub(unicode_strings, start, stop) 11.3659 11.49480 11.908870 11.71810 11.89550 20.8496   100
 substrlen(unicode_strings, start, stop, len)  9.1443  9.24115  9.604596  9.37885  9.64010 16.7211   100
       fstr_sub(unicode_strings, start, stop)  6.2186  6.47280  6.909581  6.65885  7.17215  9.9839   100
 
 > # Native multibyte (Windows-932 Japanese):
> microbenchmark::microbenchmark(
+   substrlen(test_strings, start, stop, len),
+   fstr_sub( test_strings, start, stop)
+ )
Unit: milliseconds
                                      expr     min        lq     mean   median       uq      max neval
 substrlen(test_strings, start, stop, len) 98.4097 100.23745 101.9503 100.9948 101.9676 144.7038   100
       fstr_sub(test_strings, start, stop) 42.0381  43.37445  47.2374  44.3802  47.2840  81.2012   100
 ```
_Tested on Intel(R) Core(TM) i5-8400 CPU @ 2.80GHz (6 Cores), 8 GB RAM, R version 4.0.2_
</details>


#### How `fstr_sub()` works

`fstr_sub()` follows the same routine as `fsubstr()` for positive start and stop values. On strings in variable-width and stateful character encodings where the string must be traversed from the start in order to determine the position of any given character, the following method is used for start or stop values that are negative offsets from the end of the string.
1. As the string is traversed forwards, the addresses of each character are written into a lookback ring buffer of length `m` called `last_m_char_ptrs`, wrapping around and overwriting earlier characters if the buffer length is exceeded.
2. Once the end of the string is reached, the address of the relevant character can be retrieved directly from the lookback buffer instead of having to traverse the string from the start again.

For example:

![fstr_sub routine](docs/fstr_sub.png "fstr_sub routine")

In this example, `fstr_sub(str, -6, -2)` can now be easily obtained from the lookback buffer, without having to traverse the string again. This saves time on long strings with negative offsets that are close enough to the end of the string.

A maximum lookback length is set - if the negative start/stop indices go beyond this length, then the lookback approach is not used and instead the string is traversed twice.