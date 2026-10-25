# Count observations by group

`count()` lets you quickly count the unique values of one or more
variables: `df %>% count(a, b)` is roughly equivalent to
`df %>% group_by(a, b) %>% summarise(n = n())`. `count()` is paired with
`tally()`, a lower-level helper that is equivalent to
`df %>% summarise(n = n())`. Supply `wt` to perform weighted counts,
switching the summary from from `n = n()` to `n = sum(wt)`.
`add_count()` and `add_tally()` are equivalent to `count()` and
`tally()` but use [`mutate()`](mutate.md) instead of
[`summarise()`](summarise.md) so that they add a new column with
group-wise counts.

## Usage

``` r
count(x, ..., wt = NULL, sort = FALSE, name = NULL)

tally(x, wt = NULL, sort = FALSE, name = NULL)

add_count(x, ..., wt = NULL, sort = FALSE, name = NULL)

add_tally(x, wt = NULL, sort = FALSE, name = NULL)
```

## Arguments

- x:

  A `data.frame`.

- ...:

  Variables to group by.

- wt:

  Frequency weights. If `NULL`, the default, the rows of each group are
  counted. Otherwise, the weighted count of each group is the sum of the
  (non-missing) values of `wt`, which may be a column or an expression
  of columns and variables of the calling environment, e.g.
  `wt = price * k`. A column named `n` is not used as a weight unless it
  is given as `wt`.

- sort:

  `logical(1)`. If `TRUE`, will show the largest groups at the top.

- name:

  `character(1)`. The name of the new column in the output. If omitted,
  it will default to `n`. If `n` would clash with a column that the
  output keeps, i.e. a grouping variable for `count()` and `tally()` or
  any column of the input for `add_count()` and `add_tally()`, `n` is
  repeated until the name is unique (`nn`, `nnn`, ...) and a message
  gives the name used. A column `n` that is not kept, e.g. a column
  summarised away by `count()`, does not clash, and the count is stored
  in `n`. An explicit `name` is used as given and replaces any column of
  that name.

## Value

A `data.frame`. `count()` and `add_count()` have the same groups as the
input.

## Examples

``` r
# count() is a convenient way to get a sense of the distribution of
# values in a dataset
mtcars %>% count(cyl)
#>   cyl  n
#> 1   4 11
#> 2   6  7
#> 3   8 14
mtcars %>% count(cyl, sort = TRUE)
#>   cyl  n
#> 1   8 14
#> 2   4 11
#> 3   6  7
mtcars %>% count(cyl, am, sort = TRUE)
#>   cyl am  n
#> 1   8  0 12
#> 2   4  1  8
#> 3   6  0  4
#> 4   4  0  3
#> 5   6  1  3
#> 6   8  1  2
# Note that if the data are already grouped, count() adds an additional grouping variable
# which is removed afterwards
mtcars %>% group_by(gear) %>% count(cyl)
#>   gear cyl  n
#> 1    3   4  1
#> 2    3   6  2
#> 3    3   8 12
#> 4    4   4  8
#> 5    4   6  4
#> 6    5   4  2
#> 7    5   6  1
#> 8    5   8  2

# tally() is a lower-level function that assumes you've done the grouping
mtcars %>% tally()
#>    n
#> 1 32
mtcars %>% group_by(cyl) %>% tally()
#>   cyl  n
#> 1   4 11
#> 2   6  7
#> 3   8 14

# both count() and tally() have add_ variants that work like mutate() instead of summarise
mtcars %>% add_count(cyl, wt = am)
#>                      mpg cyl  disp  hp drat    wt  qsec vs am gear carb n
#> Mazda RX4           21.0   6 160.0 110 3.90 2.620 16.46  0  1    4    4 3
#> Mazda RX4 Wag       21.0   6 160.0 110 3.90 2.875 17.02  0  1    4    4 3
#> Datsun 710          22.8   4 108.0  93 3.85 2.320 18.61  1  1    4    1 8
#> Hornet 4 Drive      21.4   6 258.0 110 3.08 3.215 19.44  1  0    3    1 3
#> Hornet Sportabout   18.7   8 360.0 175 3.15 3.440 17.02  0  0    3    2 2
#> Valiant             18.1   6 225.0 105 2.76 3.460 20.22  1  0    3    1 3
#> Duster 360          14.3   8 360.0 245 3.21 3.570 15.84  0  0    3    4 2
#> Merc 240D           24.4   4 146.7  62 3.69 3.190 20.00  1  0    4    2 8
#> Merc 230            22.8   4 140.8  95 3.92 3.150 22.90  1  0    4    2 8
#> Merc 280            19.2   6 167.6 123 3.92 3.440 18.30  1  0    4    4 3
#> Merc 280C           17.8   6 167.6 123 3.92 3.440 18.90  1  0    4    4 3
#> Merc 450SE          16.4   8 275.8 180 3.07 4.070 17.40  0  0    3    3 2
#> Merc 450SL          17.3   8 275.8 180 3.07 3.730 17.60  0  0    3    3 2
#> Merc 450SLC         15.2   8 275.8 180 3.07 3.780 18.00  0  0    3    3 2
#> Cadillac Fleetwood  10.4   8 472.0 205 2.93 5.250 17.98  0  0    3    4 2
#> Lincoln Continental 10.4   8 460.0 215 3.00 5.424 17.82  0  0    3    4 2
#> Chrysler Imperial   14.7   8 440.0 230 3.23 5.345 17.42  0  0    3    4 2
#> Fiat 128            32.4   4  78.7  66 4.08 2.200 19.47  1  1    4    1 8
#> Honda Civic         30.4   4  75.7  52 4.93 1.615 18.52  1  1    4    2 8
#> Toyota Corolla      33.9   4  71.1  65 4.22 1.835 19.90  1  1    4    1 8
#> Toyota Corona       21.5   4 120.1  97 3.70 2.465 20.01  1  0    3    1 8
#> Dodge Challenger    15.5   8 318.0 150 2.76 3.520 16.87  0  0    3    2 2
#> AMC Javelin         15.2   8 304.0 150 3.15 3.435 17.30  0  0    3    2 2
#> Camaro Z28          13.3   8 350.0 245 3.73 3.840 15.41  0  0    3    4 2
#> Pontiac Firebird    19.2   8 400.0 175 3.08 3.845 17.05  0  0    3    2 2
#> Fiat X1-9           27.3   4  79.0  66 4.08 1.935 18.90  1  1    4    1 8
#> Porsche 914-2       26.0   4 120.3  91 4.43 2.140 16.70  0  1    5    2 8
#> Lotus Europa        30.4   4  95.1 113 3.77 1.513 16.90  1  1    5    2 8
#> Ford Pantera L      15.8   8 351.0 264 4.22 3.170 14.50  0  1    5    4 2
#> Ferrari Dino        19.7   6 145.0 175 3.62 2.770 15.50  0  1    5    6 3
#> Maserati Bora       15.0   8 301.0 335 3.54 3.570 14.60  0  1    5    8 2
#> Volvo 142E          21.4   4 121.0 109 4.11 2.780 18.60  1  1    4    2 8
mtcars %>% add_tally(wt = am)
#>                      mpg cyl  disp  hp drat    wt  qsec vs am gear carb  n
#> Mazda RX4           21.0   6 160.0 110 3.90 2.620 16.46  0  1    4    4 13
#> Mazda RX4 Wag       21.0   6 160.0 110 3.90 2.875 17.02  0  1    4    4 13
#> Datsun 710          22.8   4 108.0  93 3.85 2.320 18.61  1  1    4    1 13
#> Hornet 4 Drive      21.4   6 258.0 110 3.08 3.215 19.44  1  0    3    1 13
#> Hornet Sportabout   18.7   8 360.0 175 3.15 3.440 17.02  0  0    3    2 13
#> Valiant             18.1   6 225.0 105 2.76 3.460 20.22  1  0    3    1 13
#> Duster 360          14.3   8 360.0 245 3.21 3.570 15.84  0  0    3    4 13
#> Merc 240D           24.4   4 146.7  62 3.69 3.190 20.00  1  0    4    2 13
#> Merc 230            22.8   4 140.8  95 3.92 3.150 22.90  1  0    4    2 13
#> Merc 280            19.2   6 167.6 123 3.92 3.440 18.30  1  0    4    4 13
#> Merc 280C           17.8   6 167.6 123 3.92 3.440 18.90  1  0    4    4 13
#> Merc 450SE          16.4   8 275.8 180 3.07 4.070 17.40  0  0    3    3 13
#> Merc 450SL          17.3   8 275.8 180 3.07 3.730 17.60  0  0    3    3 13
#> Merc 450SLC         15.2   8 275.8 180 3.07 3.780 18.00  0  0    3    3 13
#> Cadillac Fleetwood  10.4   8 472.0 205 2.93 5.250 17.98  0  0    3    4 13
#> Lincoln Continental 10.4   8 460.0 215 3.00 5.424 17.82  0  0    3    4 13
#> Chrysler Imperial   14.7   8 440.0 230 3.23 5.345 17.42  0  0    3    4 13
#> Fiat 128            32.4   4  78.7  66 4.08 2.200 19.47  1  1    4    1 13
#> Honda Civic         30.4   4  75.7  52 4.93 1.615 18.52  1  1    4    2 13
#> Toyota Corolla      33.9   4  71.1  65 4.22 1.835 19.90  1  1    4    1 13
#> Toyota Corona       21.5   4 120.1  97 3.70 2.465 20.01  1  0    3    1 13
#> Dodge Challenger    15.5   8 318.0 150 2.76 3.520 16.87  0  0    3    2 13
#> AMC Javelin         15.2   8 304.0 150 3.15 3.435 17.30  0  0    3    2 13
#> Camaro Z28          13.3   8 350.0 245 3.73 3.840 15.41  0  0    3    4 13
#> Pontiac Firebird    19.2   8 400.0 175 3.08 3.845 17.05  0  0    3    2 13
#> Fiat X1-9           27.3   4  79.0  66 4.08 1.935 18.90  1  1    4    1 13
#> Porsche 914-2       26.0   4 120.3  91 4.43 2.140 16.70  0  1    5    2 13
#> Lotus Europa        30.4   4  95.1 113 3.77 1.513 16.90  1  1    5    2 13
#> Ford Pantera L      15.8   8 351.0 264 4.22 3.170 14.50  0  1    5    4 13
#> Ferrari Dino        19.7   6 145.0 175 3.62 2.770 15.50  0  1    5    6 13
#> Maserati Bora       15.0   8 301.0 335 3.54 3.570 14.60  0  1    5    8 13
#> Volvo 142E          21.4   4 121.0 109 4.11 2.780 18.60  1  1    4    2 13
```
