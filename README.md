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

`fsubstr` is **25%** faster than `substr` on ASCII strings, **75%** faster for UTF-8 Unicode and **over 4.5x** the speed of `substr` on Windows-932 native encoding.


<details>
      <summary>Benchmark results</summary>

```R
> # ASCII:
> microbenchmark::microbenchmark(
+   substr( test_strings, start, stop),
+   fsubstr(test_strings, start, stop)
+ )
Unit: milliseconds
                              expr    min      lq     mean  median     uq    max neval
 substr(test_strings, start, stop) 3.7880 3.85015 4.002272 3.91205 4.0104 5.3997   100
fsubstr(test_strings, start, stop) 2.8797 3.04305 3.269372 3.11565 3.3691 4.3053   100

> # Unicode:
> microbenchmark::microbenchmark(
+   substr( unicode_strings, start, stop),
+   fsubstr(unicode_strings, start, stop)
+ )
Unit: milliseconds
                                 expr    min      lq     mean  median     uq     max neval
 substr(unicode_strings, start, stop) 8.9392 9.30175 9.469144 9.41545 9.5752 10.1570   100
fsubstr(unicode_strings, start, stop) 5.1410 5.31800 5.597086 5.40375 5.7531  7.0937   100

> # Native multibyte (Windows-932 Japanese):
> microbenchmark::microbenchmark(
+   substr( test_strings, start, stop),
+   fsubstr(test_strings, start, stop)
+ )
Unit: milliseconds
                              expr      min       lq      mean   median        uq      max neval
 substr(test_strings, start, stop) 104.7471 105.4938 106.74122 106.7004 107.52145 111.4871   100
fsubstr(test_strings, start, stop)  22.3756  22.7333  23.15743  22.8911  23.12435  30.0000   100
```
_Tested on Intel(R) Core(TM) i5-8400 CPU @ 2.80GHz (6 Cores), 8 GB RAM, R version 4.0.2_
</details>

Replacement of substrings using `fsbustr()<-` is **50%** faster than `substr()<-` for Unicode and **15%** faster for native encodings, but is **-7%** slower in the case of purely ASCII strings.

<details>
      <summary>Benchmark results</summary>

```R
> # ASCII:
> microbenchmark::microbenchmark(
+   substr( test_strings,   start, stop) <- rep_string,
+   fsubstr(test_strings_2, start, stop) <- rep_string
+ )
Unit: milliseconds
Unit: milliseconds
                                               expr     min        lq     mean    median       uq     max neval
    substr(test_strings, start, stop) <- rep_string  9.7945  9.886001 10.34820  9.945401 10.09885 41.9257   100
 fsubstr(test_strings_2, start, stop) <- rep_string 10.3580 10.493951 10.64919 10.583852 10.75350 11.3062   100
 
 > # Unicode:
 > microbenchmark::microbenchmark(
+   substr( unicode_strings,   start, stop) <- rep_unicode,
+   fsubstr(unicode_strings_2, start, stop) <- rep_unicode
+ )
Unit: milliseconds
                                                   expr     min       lq     mean  median       uq     max neval
    substr(unicode_strings, start, stop) <- rep_unicode 37.0960 37.87270 38.75429 38.3258 39.05745 59.2914   100
 fsubstr(unicode_strings_2, start, stop) <- rep_unicode 22.3753 25.32145 25.70300 25.5374 26.12995 29.7414   100
 
 > # Native multibyte (Windows-932 Japanese):
> microbenchmark::microbenchmark(
+   substr( test_strings,   start, stop) <- rep_string,
+   fsubstr(test_strings_2, start, stop) <- rep_string
+ )
Unit: milliseconds
                                               expr      min       lq     mean   median       uq      max neval
    substr(test_strings, start, stop) <- rep_string 122.1685 125.4150 127.5562 127.1847 128.5193 146.9462   100
 fsubstr(test_strings_2, start, stop) <- rep_string 107.9814 109.5362 111.1590 110.6877 111.8651 130.6164   100
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

`fstr_sub` is **80%** faster than `str_sub` on ASCII strings, **67%** faster on UTF-8 Unicode and **2.3x** the speed of `str_sub` on Windows-932 native encoding.

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
                                      expr    min      lq     mean  median     uq     max neval
        str_sub(test_strings, start, stop) 5.3253 5.56700 5.988495 5.64785 5.8461 26.7549   100
 substrlen(test_strings, start, stop, len) 3.9953 4.09755 4.664722 4.15700 4.2786 24.9957   100
       fstr_sub(test_strings, start, stop) 2.8920 3.07075 3.321851 3.14855 3.4825  4.9487   100

> # Unicode:
> microbenchmark::microbenchmark(
+   str_sub(  unicode_strings, start, stop),
+   substrlen(unicode_strings, start, stop, len),
+   fstr_sub( unicode_strings, start, stop)
+ )
Unit: milliseconds
                                         expr     min       lq      mean   median       uq     max neval
        str_sub(unicode_strings, start, stop) 11.1969 11.46355 11.910765 11.58195 11.93440 31.0321   100
 substrlen(unicode_strings, start, stop, len)  8.9586  9.25420  9.545739  9.43815  9.80315 10.5482   100
       fstr_sub(unicode_strings, start, stop)  6.4187  6.66950  7.192353  6.94520  7.53525 10.0563   100
 
 > # Native multibyte (Windows-932 Japanese):
> microbenchmark::microbenchmark(
+   substrlen(test_strings, start, stop, len),
+   fstr_sub( test_strings, start, stop)
+ )
Unit: milliseconds
                                      expr      min        lq      mean   median       uq      max neval
 substrlen(test_strings, start, stop, len) 104.9625 106.84110 108.63270 107.5460 108.7131 133.5881   100
       fstr_sub(test_strings, start, stop)  43.1571  45.17225  48.79534  46.4194  49.2254  98.1057   100
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