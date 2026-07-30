# Grouping metadata

- `group_data()` returns a data frame that defines the grouping
  structure. The columns give the values of the grouping variables. The
  last column, always called `.rows`, is a list of integer vectors that
  gives the location of the rows in each group.

&nbsp;

- `group_rows()` returns the rows which each group contains.

&nbsp;

- `group_indices()` returns an integer vector the same length as `.data`
  that gives the group that each row belongs to.

&nbsp;

- `group_vars()` gives names of grouping variables as character vector.

&nbsp;

- `groups()` gives the names as a list of symbols.

&nbsp;

- `group_size()` gives the size of each group.

&nbsp;

- `n_groups()` gives the total number of groups.

## Usage

``` r
group_data(.data)

group_rows(.data)

group_indices(.data)

group_vars(x)

groups(x)

group_size(x)

n_groups(x)
```

## Arguments

- .data, x:

  A `data.frame`.

## See also

See [context](context.md) for equivalent functions that return values
for the current group.

## Examples

``` r
df <- data.frame(x = c(1,1,2,2))
group_vars(df)
#> character(0)
group_rows(df)
#> [[1]]
#> [1] 1 2 3 4
#> 
group_data(df)
#>        .rows
#> 1 1, 2, 3, 4

gf <- group_by(df, x)
group_vars(gf)
#> [1] "x"
group_rows(gf)
#> [[1]]
#> [1] 1 2
#> 
#> [[2]]
#> [1] 3 4
#> 
group_data(gf)
#>   x .rows
#> 1 1  1, 2
#> 2 2  3, 4
```
