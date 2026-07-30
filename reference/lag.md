# Compute lagged or leading values

Find the "previous" (`lag()`) or "next" (`lead()`) values in a vector.
Useful for comparing values behind of or ahead of the current values.

## Usage

``` r
lag(x, n = 1L, default = NA)

lead(x, n = 1L, default = NA)
```

## Arguments

- x:

  A `vector` of values

- n:

  A positive `integer(1)`, giving the number of positions to lead or lag
  by.

- default:

  The value used for non-existent rows (default: `NA`).

## Examples

``` r
lag(1:5)
#> [1] NA  1  2  3  4
lead(1:5)
#> [1]  2  3  4  5 NA

x <- 1:5
data.frame(behind = lag(x), x, ahead = lead(x))
#>   behind x ahead
#> 1     NA 1     2
#> 2      1 2     3
#> 3      2 3     4
#> 4      3 4     5
#> 5      4 5    NA

# If you want to look more rows behind or ahead, use `n`
lag(1:5, n = 1)
#> [1] NA  1  2  3  4
lag(1:5, n = 2)
#> [1] NA NA  1  2  3

lead(1:5, n = 1)
#> [1]  2  3  4  5 NA
lead(1:5, n = 2)
#> [1]  3  4  5 NA NA

# If you want to define a value for non-existing rows, use `default`
lag(1:5)
#> [1] NA  1  2  3  4
lag(1:5, default = 0)
#> [1] 0 1 2 3 4

lead(1:5)
#> [1]  2  3  4  5 NA
lead(1:5, default = 6)
#> [1] 2 3 4 5 6
```
