# Get a glimpse of your data

`glimpse()` is like a transposed version of print(): columns run down
the page, and data runs across. This makes it possible to see every
column in a `data.frame`. It is no more than a wrapper around
[`utils::str()`](https://rdrr.io/r/utils/str.html) only it returns the
input (invisibly) meaning it can be used within a data pipeline.

## Usage

``` r
glimpse(x, width = getOption("width"), ...)
```

## Arguments

- x:

  An object to glimpse at.

- width:

  `integer(1)`. Width of the output.

- ...:

  Additional parameters to pass to
  [`utils::str()`](https://rdrr.io/r/utils/str.html).

## Value

`x`, invisibly.

## Examples

``` r
glimpse(mtcars)
#> 'data.frame':    32 obs. of  11 variables:
#>  $ mpg : num  21 21 22.8 21.4 18.7 18.1 14.3 24.4 22.8 19.2 ...
#>  $ cyl : num  6 6 4 6 8 6 8 4 4 6 ...
#>  $ disp: num  160 160 108 258 360 ...
#>  $ hp  : num  110 110 93 110 175 105 245 62 95 123 ...
#>  $ drat: num  3.9 3.9 3.85 3.08 3.15 2.76 3.21 3.69 3.92 3.92 ...
#>  $ wt  : num  2.62 2.88 2.32 3.21 3.44 ...
#>  $ qsec: num  16.5 17 18.6 19.4 17 ...
#>  $ vs  : num  0 0 1 1 0 1 0 1 1 1 ...
#>  $ am  : num  1 1 1 0 0 0 0 0 0 0 ...
#>  $ gear: num  4 4 4 3 3 3 3 4 4 4 ...
#>  $ carb: num  4 4 1 1 2 1 4 2 2 4 ...
```
