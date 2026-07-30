# Unite Multiple Columns Into One

Convenience function to paste together multiple columns.

## Usage

``` r
unite(data, col, ..., sep = "_", remove = TRUE, na.rm = FALSE)
```

## Arguments

- data:

  A `data.frame`.

- col:

  `character(1)` or `symbol(1)`. The name of the new column.

- ...:

  The columns to unite.

- sep:

  `character(1)`. Separator to use between the values.

- remove:

  `logical(1)`. If `TRUE`, remove the input columns from the output
  `data.frame`.

- na.rm:

  `logical(1)`. If `TRUE`, missing values will be remove prior to
  uniting each value.

## Value

A `data.frame` with the columns passed via `...` pasted together in a
new column.

## Examples

``` r
df <- data.frame(x = c("a", "a", NA, NA), y = c("b", NA, "b", NA))
df
#>      x    y
#> 1    a    b
#> 2    a <NA>
#> 3 <NA>    b
#> 4 <NA> <NA>

df %>% unite("z", x:y, remove = FALSE)
#>       z    x    y
#> 1   a_b    a    b
#> 2  a_NA    a <NA>
#> 3  NA_b <NA>    b
#> 4 NA_NA <NA> <NA>
# To remove missing values:
df %>% unite("z", x:y, na.rm = TRUE, remove = FALSE)
#>     z    x    y
#> 1 a_b    a    b
#> 2   a    a <NA>
#> 3   b <NA>    b
#> 4     <NA> <NA>
```
