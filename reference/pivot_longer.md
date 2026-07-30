# Pivot data from wide to long

`pivot_longer()` "lengthens" data, increasing the number of rows and
decreasing the number of columns. The inverse transformation is
[`pivot_wider()`](pivot_wider.md).

## Usage

``` r
pivot_longer(
  data,
  cols,
  names_to = "name",
  names_prefix = NULL,
  names_sep = NULL,
  names_pattern = NULL,
  values_to = "value",
  values_drop_na = FALSE,
  ...
)
```

## Arguments

- data:

  `data.frame`. The data to pivot.

- cols:

  \<[`poor-select`](select_helpers.md)\>. Columns to pivot into longer
  format.

- names_to:

  `character(n)`. The name of the new column(s) that will contain the
  column names.

- names_prefix:

  `character(1)`. A regular expression used to remove matching text from
  the start of each variable name.

- names_sep, names_pattern:

  `character(1)`. If `names_to` contains multiple values, this argument
  controls how the column name is broken up. `names_pattern` takes a
  regular expression containing matching groups (`()`).

- values_to:

  `character(n)`. The name of the new column(s) that will contain the
  values of the pivoted variables.

- values_drop_na:

  `logical(1)`. If `TRUE`, will drop rows that contain only `NA` in the
  `values_to` column. This effectively converts explicit missing values
  to implicit missing values, and should generally be used only when
  missing values in data were created by its structure.

- ...:

  Additional arguments passed on to methods.

## Value

A `data.frame`.

## Examples

``` r
wide_data <- data.frame(replicate(5, rnorm(10)))
# Customizing the names
pivot_longer(
  data = wide_data,
  cols = c(1, 2),
  names_to = "Column",
  values_to = "Numbers"
)
#>             X3         X4          X5 Column     Numbers
#> 1   1.78899235  0.9843769  0.52705132     X1  0.98154448
#> 2   1.78899235  0.9843769  0.52705132     X2 -0.19356013
#> 3   1.51424621  0.2540864 -0.31225835     X1 -1.87268571
#> 4   1.51424621  0.2540864 -0.31225835     X2 -0.87699220
#> 5   0.30775935 -1.2383525 -0.22312119     X1 -1.28509875
#> 6   0.30775935 -1.2383525 -0.22312119     X2  0.03921099
#> 7   1.09908832 -1.2077549  1.28323417     X1 -0.10970363
#> 8   1.09908832 -1.2077549  1.28323417     X2  0.32376765
#> 9  -1.68567376 -1.0256935 -0.33836661     X1 -0.95695466
#> 10 -1.68567376 -1.0256935 -0.33836661     X2  0.93877579
#> 11 -1.83765201 -1.5889537 -1.26325951     X1 -0.62069332
#> 12 -1.83765201 -1.5889537 -1.26325951     X2  0.15594255
#> 13 -0.05881348 -0.6766464 -0.07780695     X1  0.76654548
#> 14 -0.05881348 -0.6766464 -0.07780695     X2 -1.62802827
#> 15  0.17695824 -0.5129396  2.12530048     X1 -0.38745731
#> 16  0.17695824 -0.5129396  2.12530048     X2  0.71722359
#> 17  0.05000912 -0.9373457  0.40654532     X1 -1.54494665
#> 18  0.05000912 -0.9373457  0.40654532     X2 -1.25528683
#> 19  1.27183323 -0.2986913  1.12212716     X1  3.13227103
#> 20  1.27183323 -0.2986913  1.12212716     X2 -0.06333574
```
