# Subset distinct/unique rows

Select only distinct/unique rows from a `data.frame`.

## Usage

``` r
distinct(.data, ..., .keep_all = FALSE)
```

## Arguments

- .data:

  A `data.frame`.

- ...:

  Optional variables to use when determining uniqueness. If there are
  multiple rows for a given combination of inputs, only the first row
  will be preserved. If omitted, will use all variables.

- .keep_all:

  `logical(1)`. If `TRUE`, keep all variables in `.data`. If a
  combination of `...` is not distinct, this keeps the first row of
  values.

## Value

A `data.frame` with the following properties:

- Rows are a subset of the input but appear in the same order.

- Columns are not modified if `...` is empty or `.keep_all` is `TRUE`.
  Otherwise, `distinct()` first calls [`mutate()`](mutate.md) to create
  new columns.

- Groups are not modified.

- `data.frame` attributes are preserved.

## Examples

``` r
df <- data.frame(
  x = sample(10, 100, rep = TRUE),
  y = sample(10, 100, rep = TRUE)
)
nrow(df)
#> [1] 100
nrow(distinct(df))
#> [1] 63
nrow(distinct(df, x, y))
#> [1] 63

distinct(df, x)
#>     x
#> 1   7
#> 2   5
#> 3   9
#> 4   4
#> 5   6
#> 6   8
#> 7   3
#> 8   2
#> 9  10
#> 10  1
distinct(df, y)
#>     y
#> 1   2
#> 2  10
#> 3   8
#> 4   6
#> 5   1
#> 6   5
#> 7   9
#> 8   4
#> 9   7
#> 10  3

# You can choose to keep all other variables as well
distinct(df, x, .keep_all = TRUE)
#>     x  y
#> 1   7  2
#> 2   5 10
#> 3   9  8
#> 4   4  2
#> 5   6  6
#> 6   8  6
#> 7   3  5
#> 8   2  1
#> 9  10  2
#> 10  1  8
distinct(df, y, .keep_all = TRUE)
#>    x  y
#> 1  7  2
#> 2  5 10
#> 3  9  8
#> 4  6  6
#> 5  7  1
#> 6  3  5
#> 7  8  9
#> 8  2  4
#> 9  5  7
#> 10 9  3

# You can also use distinct on computed variables
distinct(df, diff = abs(x - y))
#>   diff
#> 1    5
#> 2    1
#> 3    2
#> 4    0
#> 5    6
#> 6    3
#> 7    7
#> 8    8
#> 9    4

# The same behaviour applies for grouped data frames,
# except that the grouping variables are always included
df <- data.frame(
  g = c(1, 1, 2, 2),
  x = c(1, 1, 2, 1)
) %>% group_by(g)
df %>% distinct(x)
#>   g x
#> 1 1 1
#> 2 2 2
#> 3 2 1
```
