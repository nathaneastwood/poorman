# Find first non-missing element

Given a set of vectors, `coalesce()` finds the first non-missing value
at each position. This is inspired by the SQL `COALESCE` function which
does the same thing for `NULL`s.

## Usage

``` r
coalesce(...)
```

## Arguments

- ...:

  Vectors. Inputs should be recyclable (either be length `1L` or `n`)
  and coercible to a common type.

## Details

Currently, `coalesce()` type checking does not take place.

## See also

[`na_if()`](na_if.md) to replace specified values to a `NA`.

[`replace_na()`](replace_na.md) to replace a `NA` with a value.

## Examples

``` r
# Use a single value to replace all missing vectors
x <- sample(c(1:5, NA, NA, NA))
coalesce(x, 0L)
#> [1] 4 0 3 0 1 5 0 2

# Or match together a complete vector from missing pieces
y <- c(1, 2, NA, NA, 5)
z <- c(NA, NA, 3, 4, 5)
coalesce(y, z)
#> [1] 1 2 3 4 5
```
