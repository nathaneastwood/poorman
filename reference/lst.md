# Build a list

`lst()` constructs a list, similar to
[`base::list()`](https://rdrr.io/r/base/list.html), but where components
are built sequentially. When defining a component, you can refer to
components created earlier in the call. `lst()` also generates missing
names automatically.

## Usage

``` r
lst(...)
```

## Arguments

- ...:

  Named or unnamed elements of a list. If the element is unnamed, its
  expression will be used as its name.

## Value

A named list.

## Examples

``` r
# the value of n can be used immediately in the definition of x
lst(n = 5, x = runif(n))
#> $n
#> [1] 5
#> 
#> $x
#> [1] 0.921526176 0.675661902 0.350588162 0.008658075 0.430898793
#> 

# missing names are constructed from user's input
lst(1:3, z = letters[4:6], runif(3))
#> $`1:3`
#> [1] 1 2 3
#> 
#> $z
#> [1] "d" "e" "f"
#> 
#> $`runif(3)`
#> [1] 0.7249351 0.4122564 0.5508624
#> 

a <- 1:3
b <- letters[4:6]
lst(a, b)
#> $a
#> [1] 1 2 3
#> 
#> $b
#> [1] "d" "e" "f"
#> 
```
