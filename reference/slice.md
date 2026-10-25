# Subset rows by position

Subset rows by their original position in the `data.frame`. Grouped
`data.frame`s use the position within each group.

## Usage

``` r
slice(.data, ...)

slice_head(.data, ..., n, prop)

slice_tail(.data, ..., n, prop)

slice_min(.data, order_by, ..., n, prop, with_ties = TRUE)

slice_max(.data, order_by, ..., n, prop, with_ties = TRUE)

slice_sample(.data, ..., n, prop, weight_by = NULL, replace = FALSE)
```

## Arguments

- .data:

  A `data.frame`.

- ...:

  For `slice()`: integer row values.

  Provide either positive values to keep, or negative values to drop.
  The values provided must be either all positive or negative. Indices
  beyond the number of rows in the input are silently ignored.

- n, prop:

  Provide either `n`, the number of rows, or `prop`, the proportion of
  rows to select. If neither are supplied, `n = 1` will be used.

  If `n` is greater than the number of rows in the group (or
  `prop > 1`), the result will be silently truncated to the group size.
  If the `prop`ortion of a group size is not an integer, it is rounded
  down.

  A negative value of `n` or `prop` gives the number or proportion of
  rows to drop from each group, e.g. `n = -2` keeps all rows but the
  last two in `slice_head()`. Any fractional part of a negative `n` is
  ignored, and the number of rows a negative `prop` drops is rounded
  down. `slice_sample()` does not accept a negative `n` or `prop`.

  `n = 0` or `prop = 0` selects no rows, as does a negative `n` or
  `prop` which drops at least as many rows as a group has. This differs
  from `dplyr` 1.0.10, where `n = 0` and `prop = 0` return every row in
  `slice_head()`, `slice_tail()`, `slice_min()` and `slice_max()` and
  are an error in `slice_sample()`, and where `slice_min()` and
  `slice_max()` with `with_ties = FALSE` can return rows when a negative
  `n` or `prop` is larger than the group. Later versions of `dplyr`
  select no rows, as `poorman` does.

- order_by:

  A variable, or an expression of the variables, to order by, e.g.
  `x * y`. It must have a value for each row of the group.

- with_ties:

  `logical(1)`. Should ties be kept together? The default, `TRUE`, may
  return more rows than you request. Use `FALSE` to ignore ties, and
  return the first `n` rows.

- weight_by:

  Sampling weights. This must evaluate to a vector of non-negative
  numbers the same length as the input, or the group on grouped data.
  Weights are automatically standardised to sum to 1.

- replace:

  `logical(1)`. Should sampling be performed with (`TRUE`) or without
  (`FALSE`, the default) replacement.

## Value

An object of the same type as `.data`. The output has the following
properties:

- Each row may appear 0, 1, or many times in the output.

- Columns are not modified.

- Groups are not modified.

- Data frame attributes are preserved.

## Examples

``` r
slice(mtcars, c(1, 2, 3))
#>                mpg cyl disp  hp drat    wt  qsec vs am gear carb
#> Mazda RX4     21.0   6  160 110 3.90 2.620 16.46  0  1    4    4
#> Mazda RX4 Wag 21.0   6  160 110 3.90 2.875 17.02  0  1    4    4
#> Datsun 710    22.8   4  108  93 3.85 2.320 18.61  1  1    4    1
mtcars %>% slice(1:3)
#>                mpg cyl disp  hp drat    wt  qsec vs am gear carb
#> Mazda RX4     21.0   6  160 110 3.90 2.620 16.46  0  1    4    4
#> Mazda RX4 Wag 21.0   6  160 110 3.90 2.875 17.02  0  1    4    4
#> Datsun 710    22.8   4  108  93 3.85 2.320 18.61  1  1    4    1

# Similar to head(mtcars, 1)
mtcars %>% slice(1L)
#>           mpg cyl disp  hp drat   wt  qsec vs am gear carb
#> Mazda RX4  21   6  160 110  3.9 2.62 16.46  0  1    4    4

# Similar to tail(mtcars, 1):
mtcars %>% slice(n())
#>             mpg cyl disp  hp drat   wt qsec vs am gear carb
#> Volvo 142E 21.4   4  121 109 4.11 2.78 18.6  1  1    4    2
mtcars %>% slice(5:n())
#>                      mpg cyl  disp  hp drat    wt  qsec vs am gear carb
#> Hornet Sportabout   18.7   8 360.0 175 3.15 3.440 17.02  0  0    3    2
#> Valiant             18.1   6 225.0 105 2.76 3.460 20.22  1  0    3    1
#> Duster 360          14.3   8 360.0 245 3.21 3.570 15.84  0  0    3    4
#> Merc 240D           24.4   4 146.7  62 3.69 3.190 20.00  1  0    4    2
#> Merc 230            22.8   4 140.8  95 3.92 3.150 22.90  1  0    4    2
#> Merc 280            19.2   6 167.6 123 3.92 3.440 18.30  1  0    4    4
#> Merc 280C           17.8   6 167.6 123 3.92 3.440 18.90  1  0    4    4
#> Merc 450SE          16.4   8 275.8 180 3.07 4.070 17.40  0  0    3    3
#> Merc 450SL          17.3   8 275.8 180 3.07 3.730 17.60  0  0    3    3
#> Merc 450SLC         15.2   8 275.8 180 3.07 3.780 18.00  0  0    3    3
#> Cadillac Fleetwood  10.4   8 472.0 205 2.93 5.250 17.98  0  0    3    4
#> Lincoln Continental 10.4   8 460.0 215 3.00 5.424 17.82  0  0    3    4
#> Chrysler Imperial   14.7   8 440.0 230 3.23 5.345 17.42  0  0    3    4
#> Fiat 128            32.4   4  78.7  66 4.08 2.200 19.47  1  1    4    1
#> Honda Civic         30.4   4  75.7  52 4.93 1.615 18.52  1  1    4    2
#> Toyota Corolla      33.9   4  71.1  65 4.22 1.835 19.90  1  1    4    1
#> Toyota Corona       21.5   4 120.1  97 3.70 2.465 20.01  1  0    3    1
#> Dodge Challenger    15.5   8 318.0 150 2.76 3.520 16.87  0  0    3    2
#> AMC Javelin         15.2   8 304.0 150 3.15 3.435 17.30  0  0    3    2
#> Camaro Z28          13.3   8 350.0 245 3.73 3.840 15.41  0  0    3    4
#> Pontiac Firebird    19.2   8 400.0 175 3.08 3.845 17.05  0  0    3    2
#> Fiat X1-9           27.3   4  79.0  66 4.08 1.935 18.90  1  1    4    1
#> Porsche 914-2       26.0   4 120.3  91 4.43 2.140 16.70  0  1    5    2
#> Lotus Europa        30.4   4  95.1 113 3.77 1.513 16.90  1  1    5    2
#> Ford Pantera L      15.8   8 351.0 264 4.22 3.170 14.50  0  1    5    4
#> Ferrari Dino        19.7   6 145.0 175 3.62 2.770 15.50  0  1    5    6
#> Maserati Bora       15.0   8 301.0 335 3.54 3.570 14.60  0  1    5    8
#> Volvo 142E          21.4   4 121.0 109 4.11 2.780 18.60  1  1    4    2
# Rows can be dropped with negative indices:
slice(mtcars, -(1:4))
#>                      mpg cyl  disp  hp drat    wt  qsec vs am gear carb
#> Hornet Sportabout   18.7   8 360.0 175 3.15 3.440 17.02  0  0    3    2
#> Valiant             18.1   6 225.0 105 2.76 3.460 20.22  1  0    3    1
#> Duster 360          14.3   8 360.0 245 3.21 3.570 15.84  0  0    3    4
#> Merc 240D           24.4   4 146.7  62 3.69 3.190 20.00  1  0    4    2
#> Merc 230            22.8   4 140.8  95 3.92 3.150 22.90  1  0    4    2
#> Merc 280            19.2   6 167.6 123 3.92 3.440 18.30  1  0    4    4
#> Merc 280C           17.8   6 167.6 123 3.92 3.440 18.90  1  0    4    4
#> Merc 450SE          16.4   8 275.8 180 3.07 4.070 17.40  0  0    3    3
#> Merc 450SL          17.3   8 275.8 180 3.07 3.730 17.60  0  0    3    3
#> Merc 450SLC         15.2   8 275.8 180 3.07 3.780 18.00  0  0    3    3
#> Cadillac Fleetwood  10.4   8 472.0 205 2.93 5.250 17.98  0  0    3    4
#> Lincoln Continental 10.4   8 460.0 215 3.00 5.424 17.82  0  0    3    4
#> Chrysler Imperial   14.7   8 440.0 230 3.23 5.345 17.42  0  0    3    4
#> Fiat 128            32.4   4  78.7  66 4.08 2.200 19.47  1  1    4    1
#> Honda Civic         30.4   4  75.7  52 4.93 1.615 18.52  1  1    4    2
#> Toyota Corolla      33.9   4  71.1  65 4.22 1.835 19.90  1  1    4    1
#> Toyota Corona       21.5   4 120.1  97 3.70 2.465 20.01  1  0    3    1
#> Dodge Challenger    15.5   8 318.0 150 2.76 3.520 16.87  0  0    3    2
#> AMC Javelin         15.2   8 304.0 150 3.15 3.435 17.30  0  0    3    2
#> Camaro Z28          13.3   8 350.0 245 3.73 3.840 15.41  0  0    3    4
#> Pontiac Firebird    19.2   8 400.0 175 3.08 3.845 17.05  0  0    3    2
#> Fiat X1-9           27.3   4  79.0  66 4.08 1.935 18.90  1  1    4    1
#> Porsche 914-2       26.0   4 120.3  91 4.43 2.140 16.70  0  1    5    2
#> Lotus Europa        30.4   4  95.1 113 3.77 1.513 16.90  1  1    5    2
#> Ford Pantera L      15.8   8 351.0 264 4.22 3.170 14.50  0  1    5    4
#> Ferrari Dino        19.7   6 145.0 175 3.62 2.770 15.50  0  1    5    6
#> Maserati Bora       15.0   8 301.0 335 3.54 3.570 14.60  0  1    5    8
#> Volvo 142E          21.4   4 121.0 109 4.11 2.780 18.60  1  1    4    2

# First and last rows based on existing order
mtcars %>% slice_head(n = 5)
#>                    mpg cyl disp  hp drat    wt  qsec vs am gear carb
#> Mazda RX4         21.0   6  160 110 3.90 2.620 16.46  0  1    4    4
#> Mazda RX4 Wag     21.0   6  160 110 3.90 2.875 17.02  0  1    4    4
#> Datsun 710        22.8   4  108  93 3.85 2.320 18.61  1  1    4    1
#> Hornet 4 Drive    21.4   6  258 110 3.08 3.215 19.44  1  0    3    1
#> Hornet Sportabout 18.7   8  360 175 3.15 3.440 17.02  0  0    3    2
mtcars %>% slice_tail(n = 5)
#>                 mpg cyl  disp  hp drat    wt qsec vs am gear carb
#> Lotus Europa   30.4   4  95.1 113 3.77 1.513 16.9  1  1    5    2
#> Ford Pantera L 15.8   8 351.0 264 4.22 3.170 14.5  0  1    5    4
#> Ferrari Dino   19.7   6 145.0 175 3.62 2.770 15.5  0  1    5    6
#> Maserati Bora  15.0   8 301.0 335 3.54 3.570 14.6  0  1    5    8
#> Volvo 142E     21.4   4 121.0 109 4.11 2.780 18.6  1  1    4    2

# Grouped operations:
mtcars %>% group_by(am, cyl, gear) %>% slice_head(n = 2)
#>                    mpg cyl  disp  hp drat    wt  qsec vs am gear carb
#> Toyota Corona     21.5   4 120.1  97 3.70 2.465 20.01  1  0    3    1
#> Merc 240D         24.4   4 146.7  62 3.69 3.190 20.00  1  0    4    2
#> Merc 230          22.8   4 140.8  95 3.92 3.150 22.90  1  0    4    2
#> Hornet 4 Drive    21.4   6 258.0 110 3.08 3.215 19.44  1  0    3    1
#> Valiant           18.1   6 225.0 105 2.76 3.460 20.22  1  0    3    1
#> Merc 280          19.2   6 167.6 123 3.92 3.440 18.30  1  0    4    4
#> Merc 280C         17.8   6 167.6 123 3.92 3.440 18.90  1  0    4    4
#> Hornet Sportabout 18.7   8 360.0 175 3.15 3.440 17.02  0  0    3    2
#> Duster 360        14.3   8 360.0 245 3.21 3.570 15.84  0  0    3    4
#> Datsun 710        22.8   4 108.0  93 3.85 2.320 18.61  1  1    4    1
#> Fiat 128          32.4   4  78.7  66 4.08 2.200 19.47  1  1    4    1
#> Porsche 914-2     26.0   4 120.3  91 4.43 2.140 16.70  0  1    5    2
#> Lotus Europa      30.4   4  95.1 113 3.77 1.513 16.90  1  1    5    2
#> Mazda RX4         21.0   6 160.0 110 3.90 2.620 16.46  0  1    4    4
#> Mazda RX4 Wag     21.0   6 160.0 110 3.90 2.875 17.02  0  1    4    4
#> Ferrari Dino      19.7   6 145.0 175 3.62 2.770 15.50  0  1    5    6
#> Ford Pantera L    15.8   8 351.0 264 4.22 3.170 14.50  0  1    5    4
#> Maserati Bora     15.0   8 301.0 335 3.54 3.570 14.60  0  1    5    8
```
