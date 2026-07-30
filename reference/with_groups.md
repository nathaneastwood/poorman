# Perform an operation with temporary groups

This function allows you to modify the grouping variables for a single
operation.

## Usage

``` r
with_groups(.data, .groups, .f, ...)
```

## Arguments

- .data:

  A `data.frame`.

- .groups:

  \<[`poor-select`](select_helpers.md)\> One or more variables to group
  by. Unlike [`group_by()`](group_by.md), you can only group by existing
  variables, and you can use `poor-select` syntax like `c(x, y, z)` to
  select multiple variables.

  Use `NULL` to temporarily **un**group.

- .f:

  A `function` to apply to regrouped data. Supports lambda-style `~`
  syntax.

- ...:

  Additional arguments passed on to `.f`.

## Examples

``` r
df <- data.frame(g = c(1, 1, 2, 2, 3), x = runif(5))
df %>% with_groups(g, mutate, x_mean = mean(x))
#>   g         x    x_mean
#> 1 1 0.3171489 0.2405161
#> 2 1 0.1638832 0.2405161
#> 3 2 0.7733527 0.7667304
#> 4 2 0.7601081 0.7667304
#> 5 3 0.5037774 0.5037774
df %>% with_groups(g, ~ mutate(.x, x_mean = mean(x)))
#>   g         x    x_mean
#> 1 1 0.3171489 0.2405161
#> 2 1 0.1638832 0.2405161
#> 3 2 0.7733527 0.7667304
#> 4 2 0.7601081 0.7667304
#> 5 3 0.5037774 0.5037774

df %>%
  group_by(g) %>%
  with_groups(NULL, mutate, x_mean = mean(x))
#>   g         x    x_mean
#> 1 1 0.3171489 0.5036541
#> 2 1 0.1638832 0.5036541
#> 3 2 0.7733527 0.5036541
#> 4 2 0.7601081 0.5036541
#> 5 3 0.5037774 0.5036541

# NB: grouping can't be restored if you remove the grouping variables
df %>%
  group_by(g) %>%
  with_groups(NULL, mutate, g = NULL)
#>           x
#> 1 0.3171489
#> 2 0.1638832
#> 3 0.7733527
#> 4 0.7601081
#> 5 0.5037774
```
