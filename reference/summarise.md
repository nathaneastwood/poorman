# Reduce multiple values down to a single value

Create one or more scalar variables summarising the variables of an
existing `data.frame`. Grouped `data.frame`s will result in one row in
the output for each group.

## Usage

``` r
summarise(.data, ..., .groups = NULL)

summarize(.data, ..., .groups = NULL)
```

## Arguments

- .data:

  A `data.frame`.

- ...:

  Name-value pairs of summary functions. The name will be the name of
  the variable in the result.

- .groups:

  `character(1)`. Grouping structure of the result.

  - `"drop_last"`: drops the last level of grouping.

  - `"drop"`: all levels of grouping are dropped.

  - `"keep"`: keeps the same grouping structure as `.data`.

  When `.groups` is not specified, it is chosen based on the number of
  rows of the results:

  - If all the results have 1 row, you get `"drop_last"`.

  - If the number of rows varies, you get `"keep"`.

  In addition, a message informs you of that choice, unless the result
  is ungrouped, the option `"poorman.summarise.inform"` is set to
  `FALSE`.

  The value can be:

  - A vector of length `1`, e.g. `min(x)`, [`n()`](context.md), or
    `sum(is.na(y))`.

  - A vector of length `n`, e.g.
    [`quantile()`](https://rdrr.io/r/stats/quantile.html).

## Details

`summarise()` and `summarize()` are synonyms.

## Examples

``` r
# A summary applied to ungrouped tbl returns a single row
mtcars %>%
  summarise(mean = mean(disp), n = n())
#>       mean  n
#> 1 230.7219 32

# Usually, you'll want to group first
mtcars %>%
  group_by(cyl) %>%
  summarise(mean = mean(disp), n = n())
#>   cyl     mean  n
#> 1   4 105.1364 11
#> 2   6 183.3143  7
#> 3   8 353.1000 14

# You can summarise to more than one value:
mtcars %>%
   group_by(cyl) %>%
   summarise(qs = quantile(disp, c(0.25, 0.75)), prob = c(0.25, 0.75))
#> `summarise()` has grouped output by 'cyl'. You can override using the `.groups` argument.
#>   cyl      x prob
#> 1   4  78.85 0.25
#> 2   4 120.65 0.75
#> 3   6 160.00 0.25
#> 4   6 196.30 0.75
#> 5   8 301.75 0.25
#> 6   8 390.00 0.75

# You use a data frame to create multiple columns so you can wrap
# this up into a function:
my_quantile <- function(x, probs) {
  data.frame(x = quantile(x, probs), probs = probs)
}
mtcars %>%
  group_by(cyl) %>%
  summarise(my_quantile(disp, c(0.25, 0.75)))
#> Error in my_quantile(disp, c(0.25, 0.75)): could not find function "my_quantile"

# Each summary call removes one grouping level (since that group
# is now just a single row)
mtcars %>%
  group_by(cyl, vs) %>%
  summarise(cyl_n = n()) %>%
  group_vars()
#> `summarise()` has grouped output by 'cyl'. You can override using the `.groups` argument.
#> [1] "cyl"
```
