# Vectorised if

This is a wrapper around
[`ifelse()`](https://rdrr.io/r/base/ifelse.html) which checks that
`true` and `false` are of the same type, making the output more
predictable.

## Usage

``` r
if_else(condition, true, false, missing = NULL)
```

## Arguments

- condition:

  A `logical(n)` vector.

- true, false:

  Values to use for `TRUE` and `FALSE` in `condition`. They must either
  be the same length as `condition` or be length 1. They must also be
  the same type.

- missing:

  If not `NULL` (the default), this will replace any missing values.

## Value

A vector the same length as `condition` with values for `TRUE` and
`FALSE` replaced by those specified in `true` and `false`, respectively.

## Examples

``` r
x <- c(-5:5, NA)
if_else(x < 0, NA_integer_, x)
#>  [1] NA NA NA NA NA  0  1  2  3  4  5 NA
if_else(x < 0, "negative", "positive", "missing")
#>  [1] "negative" "negative" "negative" "negative" "negative" "positive"
#>  [7] "positive" "positive" "positive" "positive" "positive" "missing" 

# Unlike ifelse, if_else preserves types
x <- factor(sample(letters[1:5], 10, replace = TRUE))
ifelse(x %in% c("a", "b", "c"), x, factor(NA))
#>  [1]  3  2  3  3  1 NA  1 NA  3  1
# Attributes are taken from the `true` vector
if_else(x %in% c("a", "b", "c"), x, factor(NA))
#>  [1] c    b    c    c    a    <NA> a    <NA> c    a   
#> Levels: a b c d e
```
