# Nest Join

`nest_join()` keeps every row and column of `x` and adds one list
column. Each element of that column is a `data.frame` holding the rows
of `y` whose keys match the row of `x`. A row of `x` without a match in
`y` gets a `data.frame` with zero rows.

## Usage

``` r
nest_join(x, y, by = NULL, copy = FALSE, keep = FALSE, name = NULL, ...)
```

## Arguments

- x, y:

  The `data.frame`s to join.

- by:

  A character vector naming the key columns. With `NULL`, the default,
  the keys are every column name that `x` and `y` share, and a message
  reports them; pass `by` yourself to silence the message.

  Name the elements of `by` when the keys have different names in the
  two tables: `by = c("a" = "b")` pairs column `a` of `x` with column
  `b` of `y`.

  `by = character()` uses no keys, so every row of `x` is paired with
  the whole of `y`.

- copy:

  Ignored. Included for compatibility with `dplyr`, where it allows `y`
  to come from a different data source.

- keep:

  `logical(1)`. Should the key columns of `y` also appear in the nested
  `data.frame`s?

- name:

  `character(1)`. The name of the new list column. With `NULL`, the
  default, the column is named after the expression passed as `y`,
  abbreviated as the automatic names of [`summarise()`](summarise.md).

- ...:

  Ignored. Included for compatibility with `dplyr`.

## Value

A `data.frame` with the rows and columns of `x` and a list column,
`name`, of `data.frame`s. The join keys of `x` are coerced to the common
type of the keys in `x` and `y`. Groups are taken from `x`.

## Details

As rows of `x` are never dropped or repeated, the result always has as
many rows as `x`. The other joins can be built on top of it. Counting
the rows of each nested `data.frame` tells you which rows
[`semi_join()`](filter_joins.md) and [`anti_join()`](filter_joins.md)
would keep, and expanding the nested rows back out gives the result of
[`inner_join()`](mutate_joins.md) or, if the empty matches become
missing values, [`left_join()`](mutate_joins.md).

## Examples

``` r
customers <- data.frame(id = 1:3, customer = c("Ana", "Ben", "Chloe"))
orders <- data.frame(id = c(1L, 1L, 3L, 4L), item = c("tea", "jam", "bread", "milk"))

# Ben (id 2) has no orders, so gets a zero row data.frame
res <- nest_join(customers, orders, by = "id")
res$orders
#> [[1]]
#>   item
#> 1  tea
#> 2  jam
#> 
#> [[2]]
#> [1] item
#> <0 rows> (or 0-length row.names)
#> 
#> [[3]]
#>    item
#> 1 bread
#> 

# Keep the key column in the nested data and choose the column name
nest_join(customers, orders, by = "id", keep = TRUE, name = "purchases")$purchases
#> [[1]]
#>   id item
#> 1  1  tea
#> 2  1  jam
#> 
#> [[2]]
#> [1] id   item
#> <0 rows> (or 0-length row.names)
#> 
#> [[3]]
#>   id  item
#> 1  3 bread
#> 

# Counting the orders of each customer gives semi_join() and anti_join()
n_orders <- vapply(res$orders, nrow, integer(1L))
res[n_orders > 0L, c("id", "customer")]  # semi_join(customers, orders, by = "id")
#>   id customer
#> 1  1      Ana
#> 3  3    Chloe
res[n_orders == 0L, c("id", "customer")] # anti_join(customers, orders, by = "id")
#>   id customer
#> 2  2      Ben

# Expanding the orders back out gives inner_join()
expand_matches <- function(x, nested) {
  n <- vapply(nested, nrow, integer(1L))
  cbind(x[rep(seq_len(nrow(x)), n), , drop = FALSE], do.call(rbind, nested), row.names = NULL)
}
expand_matches(res[c("id", "customer")], res$orders) # inner_join(customers, orders, by = "id")
#>   id customer  item
#> 1  1      Ana   tea
#> 2  1      Ana   jam
#> 3  3    Chloe bread

# and left_join() once each empty match becomes a row of missing values
filled <- lapply(
  res$orders,
  function(o) if (nrow(o) == 0L) o[NA_integer_, , drop = FALSE] else o
)
expand_matches(res[c("id", "customer")], filled) # left_join(customers, orders, by = "id")
#>   id customer  item
#> 1  1      Ana   tea
#> 2  1      Ana   jam
#> 3  2      Ben  <NA>
#> 4  3    Chloe bread
```
