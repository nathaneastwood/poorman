# Count the number of unique values in a set of vectors

This is the equivalent of `length(unique(x))` for multiple vectors.

## Usage

``` r
n_distinct(..., na.rm = FALSE)
```

## Arguments

- ...:

  Vectors of values.

- na.rm:

  `logical(1)`. If `TRUE` missing values don't count.

## Examples

``` r
x <- sample(1:10, 1e5, rep = TRUE)
length(unique(x))
#> [1] 10
n_distinct(x)
#> [1] 10
```
