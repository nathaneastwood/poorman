# Windowed Rank Functions

Six variations on ranking functions, mimicking the ranking functions
described in SQL2003. They are currently implemented using the built in
[`rank()`](https://rdrr.io/r/base/rank.html) function. All ranking
functions map smallest inputs to smallest outputs. Use
[`desc()`](desc.md) to reverse the direction.

## Usage

``` r
cume_dist(x)

dense_rank(x)

min_rank(x)

ntile(x = row_number(), n)

percent_rank(x)

row_number(x)
```

## Arguments

- x:

  A vector of values to rank. Missing values are left as is. If you want
  to treat them as the smallest or largest values, replace with `Inf` or
  `-Inf` before ranking.

- n:

  `integer(1)`. The number of groups to split up into.

## Details

- `cume_dist()`: a cumulative distribution function. Proportion of all
  values less than or equal to the current rank.

- `dense_rank()`: like `min_rank()`, but with no gaps between ranks

- `min_rank()`: equivalent to `rank(ties.method = "min")`

- `ntile()`: a rough rank, which breaks the input vector into `n`
  buckets. The size of the buckets may differ by up to one, larger
  buckets have lower rank.

- `percent_rank()`: a number between `0` and `1` computed by rescaling
  `min_rank` to `[0, 1]`

- `row_number()`: equivalent to `rank(ties.method = "first")`

## Examples

``` r
x <- c(5, 1, 3, 2, 2, NA)
row_number(x)
#> [1]  5  1  4  2  3 NA
min_rank(x)
#> [1]  5  1  4  2  2 NA
dense_rank(x)
#> [1]  4  1  3  2  2 NA
percent_rank(x)
#> [1] 1.00 0.00 0.75 0.25 0.25   NA
cume_dist(x)
#> [1] 1.0 0.2 0.8 0.6 0.6  NA

ntile(x, 2)
#> [1]  2  1  2  1  1 NA
ntile(1:8, 3)
#> [1] 1 1 1 2 2 2 3 3

# row_number can be used with single table verbs without specifying x
# (for data frames and databases that support windowing)
mutate(mtcars, row_number() == 1L)
#>                      mpg cyl  disp  hp drat    wt  qsec vs am gear carb
#> Mazda RX4           21.0   6 160.0 110 3.90 2.620 16.46  0  1    4    4
#> Mazda RX4 Wag       21.0   6 160.0 110 3.90 2.875 17.02  0  1    4    4
#> Datsun 710          22.8   4 108.0  93 3.85 2.320 18.61  1  1    4    1
#> Hornet 4 Drive      21.4   6 258.0 110 3.08 3.215 19.44  1  0    3    1
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
#>                     row_number() == 1L
#> Mazda RX4                         TRUE
#> Mazda RX4 Wag                    FALSE
#> Datsun 710                       FALSE
#> Hornet 4 Drive                   FALSE
#> Hornet Sportabout                FALSE
#> Valiant                          FALSE
#> Duster 360                       FALSE
#> Merc 240D                        FALSE
#> Merc 230                         FALSE
#> Merc 280                         FALSE
#> Merc 280C                        FALSE
#> Merc 450SE                       FALSE
#> Merc 450SL                       FALSE
#> Merc 450SLC                      FALSE
#> Cadillac Fleetwood               FALSE
#> Lincoln Continental              FALSE
#> Chrysler Imperial                FALSE
#> Fiat 128                         FALSE
#> Honda Civic                      FALSE
#> Toyota Corolla                   FALSE
#> Toyota Corona                    FALSE
#> Dodge Challenger                 FALSE
#> AMC Javelin                      FALSE
#> Camaro Z28                       FALSE
#> Pontiac Firebird                 FALSE
#> Fiat X1-9                        FALSE
#> Porsche 914-2                    FALSE
#> Lotus Europa                     FALSE
#> Ford Pantera L                   FALSE
#> Ferrari Dino                     FALSE
#> Maserati Bora                    FALSE
#> Volvo 142E                       FALSE
mtcars %>% filter(between(row_number(), 1, 10))
#>                    mpg cyl  disp  hp drat    wt  qsec vs am gear carb
#> Mazda RX4         21.0   6 160.0 110 3.90 2.620 16.46  0  1    4    4
#> Mazda RX4 Wag     21.0   6 160.0 110 3.90 2.875 17.02  0  1    4    4
#> Datsun 710        22.8   4 108.0  93 3.85 2.320 18.61  1  1    4    1
#> Hornet 4 Drive    21.4   6 258.0 110 3.08 3.215 19.44  1  0    3    1
#> Hornet Sportabout 18.7   8 360.0 175 3.15 3.440 17.02  0  0    3    2
#> Valiant           18.1   6 225.0 105 2.76 3.460 20.22  1  0    3    1
#> Duster 360        14.3   8 360.0 245 3.21 3.570 15.84  0  0    3    4
#> Merc 240D         24.4   4 146.7  62 3.69 3.190 20.00  1  0    4    2
#> Merc 230          22.8   4 140.8  95 3.92 3.150 22.90  1  0    4    2
#> Merc 280          19.2   6 167.6 123 3.92 3.440 18.30  1  0    4    4
```
