# Mutating Joins

The mutating joins add columns from `y` to `x`, matching rows based on
the keys:

- `inner_join()`: includes all rows in `x` and `y`.

- `left_join()`: includes all rows in `x`.

- `right_join()`: includes all rows in `y`.

- `full_join()`: includes all rows in `x` or `y`.

If a row in `x` matches multiple rows in `y`, all the rows in `y` will
be returned once for each matching row in `x`.

## Usage

``` r
inner_join(
  x,
  y,
  by = NULL,
  suffix = c(".x", ".y"),
  ...,
  na_matches = c("na", "never")
)

left_join(
  x,
  y,
  by = NULL,
  suffix = c(".x", ".y"),
  ...,
  keep = FALSE,
  na_matches = c("na", "never")
)

right_join(
  x,
  y,
  by = NULL,
  suffix = c(".x", ".y"),
  ...,
  keep = FALSE,
  na_matches = c("na", "never")
)

full_join(
  x,
  y,
  by = NULL,
  suffix = c(".x", ".y"),
  ...,
  keep = FALSE,
  na_matches = c("na", "never")
)
```

## Arguments

- x, y:

  The `data.frame`s to join.

- by:

  A character vector of variables to join by. If `NULL`, the default,
  `*_join()` will do a natural join, using all variables with common
  names across the two tables. A message lists the variables so that you
  can check they're right (to suppress the message, simply explicitly
  list the variables that you want to join).

  To join by different variables on x and y use a named vector. For
  example, `by = c("a" = "b")` will match `x.a` to `y.b`.

  To join by multiple variables, use a vector with length \> 1. For
  example, `by = c("a", "b")` will match `x$a` to `y$a` and `x$b` to
  `y$b`. Use a named vector to match different variables in `x` and `y`.
  For example, `by = c("a" = "b", "c" = "d")` will match `x$a` to `y$b`
  and `x$c` to `y$d`.

  To perform a cross-join, generating all combinations of `x` and `y`,
  use `by = character()`.

- suffix:

  `character(2)`. If there are non-joined duplicate variables in `x` and
  `y`, these suffixes will be added to the output to disambiguate them.

- ...:

  Additional arguments to pass to
  [`merge()`](https://rdrr.io/r/base/merge.html)

- na_matches:

  Should `NA` and `NaN` values match one another?

  The default, `"na"`, treats two `NA` or `NaN` values as equal, like
  `%in%`, [`match()`](https://rdrr.io/r/base/match.html),
  [`merge()`](https://rdrr.io/r/base/merge.html).

  Use `"never"` to always treat two `NA` or `NaN` values as different,
  like joins for database sources, similarly to
  `merge(incomparables = FALSE)`.

- keep:

  `logical(1)`. Should the join keys from both `x` and `y` be preserved
  in the output? Only applies to `left_join()`, `right_join()`, and
  `full_join()`.

## Value

A `data.frame`. The order of the rows and columns of `x` is preserved as
much as possible. The output has the following properties:

- For `inner_join()`, a subset of `x` rows. For `left_join()`, all `x`
  rows. For `right_join()`, a subset of `x` rows, followed by unmatched
  `y` rows. For `full_join()`, all `x` rows, followed by unmatched `y`
  rows.

- For all joins, rows will be duplicated if one or more rows in `x`
  matches multiple rows in `y`.

- Output columns include all `x` columns and all `y` columns. If columns
  in `x` and `y` have the same name (and aren't included in `by`),
  `suffix`es are added to disambiguate.

- Output columns included in `by` are coerced to common type across `x`
  and `y`.

- Groups are taken from `x`.

## Examples

``` r
# If a row in `x` matches multiple rows in `y`, all the rows in `y` will be
# returned once for each matching row in `x`
df1 <- data.frame(x = 1:3)
df2 <- data.frame(x = c(1, 1, 2), y = c("first", "second", "third"))
df1 %>% left_join(df2)
#> Joining, by = "x"
#>   x      y
#> 1 1  first
#> 2 1 second
#> 3 2  third
#> 4 3   <NA>

# By default, NAs match other NAs so that there are two
# rows in the output of this join:
df1 <- data.frame(x = c(1, NA), y = 2)
df2 <- data.frame(x = c(1, NA), z = 3)
left_join(df1, df2)
#> Joining, by = "x"
#>    x y z
#> 1  1 2 3
#> 2 NA 2 3

# You can optionally request that NAs don't match, giving a
# a result that more closely resembles SQL joins
left_join(df1, df2, na_matches = "never")
#> Joining, by = "x"
#>    x y  z
#> 1  1 2  3
#> 2 NA 2 NA
```
