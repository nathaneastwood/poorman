# Filtering joins filter rows from `x` based on the presence or absence of matches in `y`:

- `semi_join()` return all rows from `x` with a match in `y`.

- `anti_join()` return all rows from `x` with*out* a match in `y`.

## Usage

``` r
anti_join(x, y, by = NULL)

semi_join(x, y, by = NULL)
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

## Examples

``` r
table1 <- data.frame(
  pupil = rep(1:3, each = 2),
  test = rep(c("A", "B"), 3),
  score = c(60, 70, 65, 80, 85, 70),
  stringsAsFactors = FALSE
)
table2 <- table1[c(1, 3, 4), ]

table1 %>% anti_join(table2, by = c("pupil", "test"))
#>   pupil test score
#> 1     1    B    70
#> 2     3    A    85
#> 3     3    B    70
table1 %>% semi_join(table2, by = c("pupil", "test"))
#>   pupil test score
#> 1     1    A    60
#> 2     2    A    65
#> 3     2    B    80
```
