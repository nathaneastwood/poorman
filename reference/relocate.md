# Change column order

Use `relocate()` to change column positions, using the same syntax as
[`select()`](select.md) to make it easy to move blocks of columns at
once.

## Usage

``` r
relocate(.data, ..., .before = NULL, .after = NULL)
```

## Arguments

- .data:

  A `data.frame`.

- ...:

  \<[`poor-select`](select_helpers.md)\> Columns to move.

- .before, .after:

  \<[`poor-select`](select_helpers.md)\> Destination of columns selected
  by `...`. Supplying neither will move columns to the left-hand side;
  specifying both will result in an error.

## Value

An object of the same type as `.data`. The output has the following
properties:

- Rows are not affected.

- The same columns appear in the output, but (usually) in a different
  place.

- Data frame attributes are preserved.

- Groups are not affected.

## Examples

``` r
df <- data.frame(
  a = 1, b = 1, c = 1, d = "a", e = "a", f = "a",
  stringsAsFactors = FALSE
)
df %>% relocate(f)
#>   f a b c d e
#> 1 a 1 1 1 a a
df %>% relocate(a, .after = c)
#>   b c a d e f
#> 1 1 1 1 a a a
df %>% relocate(f, .before = b)
#>   a f b c d e
#> 1 1 a 1 1 a a
df %>% relocate(a, .after = last_col())
#>   b c d e f a
#> 1 1 1 a a a 1

# Can also select variables based on their type
df %>% relocate(where(is.character))
#>   d e f a b c
#> 1 a a a 1 1 1
df %>% relocate(where(is.numeric), .after = last_col())
#>   d e f a b c
#> 1 a a a 1 1 1
# Or with any other select helper
df %>% relocate(any_of(c("a", "e", "i", "o", "u")))
#>   a e b c d f
#> 1 1 a 1 1 a a

# When .before or .after refers to multiple variables they will be
# moved to be immediately before/after the selected variables.
df2 <- data.frame(
  a = 1, b = "a", c = 1, d = "a",
  stringsAsFactors = FALSE
)
df2 %>% relocate(where(is.numeric), .after = where(is.character))
#>   b d a c
#> 1 a a 1 1
df2 %>% relocate(where(is.numeric), .before = where(is.character))
#>   a c b d
#> 1 1 1 a a
```
