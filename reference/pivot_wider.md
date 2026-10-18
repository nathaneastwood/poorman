# Pivot data from long to wide

`pivot_wider()` "widens" data, increasing the number of columns and
decreasing the number of rows. The inverse transformation is
[`pivot_longer()`](pivot_longer.md).

## Usage

``` r
pivot_wider(
  data,
  id_cols = NULL,
  names_from = "Name",
  names_prefix = "",
  names_sep = "_",
  names_glue = NULL,
  names_sort = FALSE,
  names_repair = "check_unique",
  values_from = "Value",
  values_fill = NULL,
  values_fn = NULL,
  ...
)
```

## Arguments

- data:

  `data.frame`. The data to pivot.

- id_cols:

  \<[`poor-select`](select_helpers.md)\>. The columns that uniquely
  identify each observation. Defaults to all columns in `data` except
  for the columns specified in `names_from` and `values_from`.

- names_from:

  \<[`poor-select`](select_helpers.md)\>. The column(s) that contain the
  levels to be used as future column names.

- names_prefix:

  `character(1)`. String added to the start of every variable name. This
  is particularly useful if `names_from` is a numeric vector and you
  want to create syntactic variable names.

- names_sep:

  `character(1)`. If `names_from` or `values_from` contains multiple
  variables, this will be used to join their values together into a
  single string to use as a column name.

- names_glue:

  `character(1)`. Instead of `names_sep` and `names_prefix`, you can
  supply a [glue specification](https://glue.tidyverse.org/index.html)
  that uses the `names_from` columns to create custom column names. Note
  that the only delimiters supported by `names_glue` are curly brackets,
  `{` and `}`.

- names_sort:

  `logical(1)`. Should the column names be sorted? If `FALSE`, the
  default, column names are ordered by first appearance. Factors are
  sorted by their levels and characters in the C locale, with `NA` last.

- names_repair:

  `character(1)` or `function`. What happens if the output has invalid
  column names. The default, `"check_unique"`, is to error if the
  columns are duplicated. Use `"minimal"` to allow duplicates in the
  output, `"unique"` to de-duplicate by adding numeric suffixes or
  `"universal"` to also make the names syntactic. A function is applied
  to the names.

- values_from:

  \<[`poor-select`](select_helpers.md)\>. The column(s) that contain the
  values to be used as future variable values. If the values are not
  uniquely identified by the `id_cols` and `names_from` columns, the
  output will contain list-columns and a warning is given.

- values_fill:

  Optionally, a scalar value that will be used to fill in missing
  values, i.e. combinations of `id_cols` and `names_from` which are not
  present in `data`. Explicit `NA`s in `values_from` are not replaced.
  This can also be a named list, specifying a fill value for each of the
  `values_from` columns.

- values_fn:

  `function` or named [`list()`](https://rdrr.io/r/base/list.html).
  Optionally, a function applied to the value in each cell in the
  output, typically to summarise values which are not uniquely
  identified, e.g. `values_fn = mean`. A named list gives a function for
  each `values_from` column, where functions may also be given as
  formulas, e.g. `list(value = ~ max(.x))`. Use `values_fn = list` to
  create list-columns without a warning.

- ...:

  Not used for now.

## Value

If a tibble was provided as input, `pivot_wider()` also returns a
tibble. Otherwise, it returns a data frame.

## Examples

``` r
data_long <- read.table(header = TRUE, text = "
 subject sex condition measurement
       1   M   control         7.9
       1   M     cond1        12.3
       1   M     cond2        10.7
       2   F   control         6.3
       2   F     cond1        10.6
       2   F     cond2        11.1
       3   F   control         9.5
       3   F     cond1        13.1
       3   F     cond2        13.8
       4   M   control        11.5
       4   M     cond1        13.4
       4   M     cond2        12.9")


pivot_wider(
  data_long,
  id_cols = "subject",
  names_from = "condition",
  values_from = "measurement"
)
#>   subject control cond1 cond2
#> 1       1     7.9  12.3  10.7
#> 2       2     6.3  10.6  11.1
#> 3       3     9.5  13.1  13.8
#> 4       4    11.5  13.4  12.9

pivot_wider(
  data_long,
  id_cols = "subject",
  names_from = "condition",
  values_from = "measurement",
  names_prefix = "Var.",
  names_sep = "."
)
#>   subject Var.control Var.cond1 Var.cond2
#> 1       1         7.9      12.3      10.7
#> 2       2         6.3      10.6      11.1
#> 3       3         9.5      13.1      13.8
#> 4       4        11.5      13.4      12.9

production <- expand.grid(
  product = c("A", "B"),
  country = c("AI", "EI"),
  year = 2000:2014
) %>%
  filter((product == "A" & country == "AI") | product == "B") %>%
  mutate(production = rnorm(nrow(.)))

pivot_wider(
  production,
  names_from = c("product", "country"),
  values_from = "production",
  names_glue = "prod_{product}_{country}"
)
#>    year   prod_A_AI  prod_B_AI   prod_B_EI
#> 1  2000 -0.24399241  0.4353313 -0.69475390
#> 2  2001  1.40735841  0.6640163 -0.17235734
#> 3  2002  0.24208579  0.4777955 -1.55287301
#> 4  2003  0.47450988 -0.5638155 -2.81039662
#> 5  2004  0.02122596  0.1511730  0.93459111
#> 6  2005 -1.20202066 -0.2370773 -0.54026076
#> 7  2006 -0.57998724  0.4633448  0.52688060
#> 8  2007 -0.14132877  0.9663687 -0.99768681
#> 9  2008 -0.85391920 -1.6952953 -0.30505267
#> 10 2009  1.19629139 -0.2538215  0.56936693
#> 11 2010  0.39035750  0.3673242  0.07176328
#> 12 2011 -1.06431585 -0.8007223 -0.32601133
#> 13 2012  0.95643219 -1.1394993 -0.11804413
#> 14 2013  0.16399451 -0.1356509  1.25739238
#> 15 2014  0.08610159 -1.3601053  0.45949303
```
