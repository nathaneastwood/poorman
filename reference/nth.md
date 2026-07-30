# Extract the first, last or nth value from a vector

These are straightforward wrappers around
[`[[`](https://rdrr.io/r/base/Extract.html). The main advantage is that
you can provide an optional secondary vector that defines the ordering,
and provide a default value to use when the input is shorter than
expected.

## Usage

``` r
nth(x, n, order_by = NULL, default = default_missing(x))

first(x, order_by = NULL, default = default_missing(x))

last(x, order_by = NULL, default = default_missing(x))
```

## Arguments

- x:

  A vector

- n:

  For `nth()`, a single integer specifying the position. Negative
  integers index from the end (i.e. `-1L` will return the last value in
  the vector).

  If a double is supplied, it will be silently truncated.

- order_by:

  An optional vector used to determine the order

- default:

  A default value to use if the position does not exist in the input.
  This is guessed by default for base vectors, where a missing value of
  the appropriate type is returned, and for lists, where a `NULL` is
  return.

  For more complicated objects, you'll need to supply this value. Make
  sure it is the same type as `x`.

## Value

A single value. `[[` is used to do the subsetting.

## Examples

``` r
x <- 1:10
y <- 10:1

first(x)
#> [1] 1
last(y)
#> [1] 1

nth(x, 1)
#> [1] 1
nth(x, 5)
#> [1] 5
nth(x, -2)
#> [1] 9
nth(x, 11)
#> [1] NA

last(x)
#> [1] 10
# Second argument provides optional ordering
last(x, y)
#> [1] 1

# These functions always return a single value
first(integer())
#> [1] NA
```
