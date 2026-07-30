# Replace missing values

Replace missing values in a `data.frame` or `vector`.

## Usage

``` r
replace_na(data, replace, ...)
```

## Arguments

- data:

  A `data.frame` or `vector`.

- replace:

  If `data` is a `data.frame`, a named `list` giving the value to
  replace `NA` with for each column. If `data` is a `vector`, a single
  value used for replacement.

- ...:

  Additional arguments passed onto methods; not currently used.

## Value

If `data` is a `data.frame`, `replace_na()` returns a `data.frame`. If
`data` is a `vector`, `replace_na()` returns a `vector` of class
determined by the union of `data` and `replace`.

## See also

[`na_if()`](na_if.md) to replace specified values with a `NA`.

[`coalesce()`](coalesce.md) to replace missing values within subsequent
`vector`(s) of value(s).

## Examples

``` r
df <- data.frame(x = c(1, 2, NA), y = c("a", NA, "b"), stringsAsFactors = FALSE)
df %>% replace_na(list(x = 0, y = "unknown"))
#>   x       y
#> 1 1       a
#> 2 2 unknown
#> 3 0       b
df %>% mutate(x = replace_na(x, 0))
#>   x    y
#> 1 1    a
#> 2 2 <NA>
#> 3 0    b

df$x %>% replace_na(0)
#> [1] 1 2 0
df$y %>% replace_na("unknown")
#> [1] "a"       "unknown" "b"      
```
