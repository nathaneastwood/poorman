# Create or transform variables

`mutate()` adds new variables and preserves existing ones; `transmute()`
adds new variables and drops existing ones. Both functions preserve the
number of rows of the input. New variables overwrite existing variables
of the same name. Variables can be removed by setting their value to
`NULL`.

## Usage

``` r
mutate(.data, ...)

# S3 method for class 'data.frame'
mutate(
  .data,
  ...,
  .keep = c("all", "used", "unused", "none"),
  .before = NULL,
  .after = NULL
)

transmute(.data, ...)
```

## Arguments

- .data:

  A `data.frame`.

- ...:

  Name-value pairs of expressions, each with length `1L`. The name of
  each argument will be the name of a new column and the value will be
  its corresponding value. Use a `NULL` value in `mutate` to drop a
  variable. New variables overwrite existing variables of the same name.

  A name can also be given with `:=`, e.g. `total := sum(x)`. A string
  on the left of `:=` is a `gluestick` template which is filled in from
  the calling environment, so that a name can be built from a variable,
  e.g. `"{var}_mean" := mean(x)`.

- .keep:

  This argument allows you to control which columns from `.data` are
  retained in the output:

  - `"all"`, the default, retains all variables.

  - `"used"` keeps any variables used to make new variables; it's useful
    for checking your work as it displays inputs and outputs
    side-by-side.

  - `"unused"` keeps only existing variables **not** used to make new
    variables.

  - `"none"`, only keeps grouping keys (like `transmute()`).

  Grouping variables are always kept, unconditional to `.keep`.

- .before, .after:

  \<[`poor-select`](select_helpers.md)\> Optionally, control where new
  columns should appear (the default is to add to the right hand side).
  See [`relocate()`](relocate.md) for more details.

## Useful mutate functions

- [`+`](https://rdrr.io/r/base/Arithmetic.html),
  [`-`](https://rdrr.io/r/base/Arithmetic.html),
  [`log()`](https://rdrr.io/r/base/Log.html), etc., for their usual
  mathematical meanings

- [`lead()`](lag.md), [`lag()`](lag.md)

- [`dense_rank()`](window_rank.md), [`min_rank()`](window_rank.md),
  [`percent_rank()`](window_rank.md), [`row_number()`](window_rank.md),
  [`cume_dist()`](window_rank.md), [`ntile()`](window_rank.md)

- [`cumsum()`](https://rdrr.io/r/base/cumsum.html),
  [`cummin()`](https://rdrr.io/r/base/cumsum.html),
  [`cummax()`](https://rdrr.io/r/base/cumsum.html)

- [`na_if()`](na_if.md), [`coalesce()`](coalesce.md)

- [`if_else()`](if_else.md), [`recode()`](recode.md),
  [`case_when()`](case_when.md)

## Examples

``` r
mutate(mtcars, mpg2 = mpg * 2)
#>                      mpg cyl  disp  hp drat    wt  qsec vs am gear carb mpg2
#> Mazda RX4           21.0   6 160.0 110 3.90 2.620 16.46  0  1    4    4 42.0
#> Mazda RX4 Wag       21.0   6 160.0 110 3.90 2.875 17.02  0  1    4    4 42.0
#> Datsun 710          22.8   4 108.0  93 3.85 2.320 18.61  1  1    4    1 45.6
#> Hornet 4 Drive      21.4   6 258.0 110 3.08 3.215 19.44  1  0    3    1 42.8
#> Hornet Sportabout   18.7   8 360.0 175 3.15 3.440 17.02  0  0    3    2 37.4
#> Valiant             18.1   6 225.0 105 2.76 3.460 20.22  1  0    3    1 36.2
#> Duster 360          14.3   8 360.0 245 3.21 3.570 15.84  0  0    3    4 28.6
#> Merc 240D           24.4   4 146.7  62 3.69 3.190 20.00  1  0    4    2 48.8
#> Merc 230            22.8   4 140.8  95 3.92 3.150 22.90  1  0    4    2 45.6
#> Merc 280            19.2   6 167.6 123 3.92 3.440 18.30  1  0    4    4 38.4
#> Merc 280C           17.8   6 167.6 123 3.92 3.440 18.90  1  0    4    4 35.6
#> Merc 450SE          16.4   8 275.8 180 3.07 4.070 17.40  0  0    3    3 32.8
#> Merc 450SL          17.3   8 275.8 180 3.07 3.730 17.60  0  0    3    3 34.6
#> Merc 450SLC         15.2   8 275.8 180 3.07 3.780 18.00  0  0    3    3 30.4
#> Cadillac Fleetwood  10.4   8 472.0 205 2.93 5.250 17.98  0  0    3    4 20.8
#> Lincoln Continental 10.4   8 460.0 215 3.00 5.424 17.82  0  0    3    4 20.8
#> Chrysler Imperial   14.7   8 440.0 230 3.23 5.345 17.42  0  0    3    4 29.4
#> Fiat 128            32.4   4  78.7  66 4.08 2.200 19.47  1  1    4    1 64.8
#> Honda Civic         30.4   4  75.7  52 4.93 1.615 18.52  1  1    4    2 60.8
#> Toyota Corolla      33.9   4  71.1  65 4.22 1.835 19.90  1  1    4    1 67.8
#> Toyota Corona       21.5   4 120.1  97 3.70 2.465 20.01  1  0    3    1 43.0
#> Dodge Challenger    15.5   8 318.0 150 2.76 3.520 16.87  0  0    3    2 31.0
#> AMC Javelin         15.2   8 304.0 150 3.15 3.435 17.30  0  0    3    2 30.4
#> Camaro Z28          13.3   8 350.0 245 3.73 3.840 15.41  0  0    3    4 26.6
#> Pontiac Firebird    19.2   8 400.0 175 3.08 3.845 17.05  0  0    3    2 38.4
#> Fiat X1-9           27.3   4  79.0  66 4.08 1.935 18.90  1  1    4    1 54.6
#> Porsche 914-2       26.0   4 120.3  91 4.43 2.140 16.70  0  1    5    2 52.0
#> Lotus Europa        30.4   4  95.1 113 3.77 1.513 16.90  1  1    5    2 60.8
#> Ford Pantera L      15.8   8 351.0 264 4.22 3.170 14.50  0  1    5    4 31.6
#> Ferrari Dino        19.7   6 145.0 175 3.62 2.770 15.50  0  1    5    6 39.4
#> Maserati Bora       15.0   8 301.0 335 3.54 3.570 14.60  0  1    5    8 30.0
#> Volvo 142E          21.4   4 121.0 109 4.11 2.780 18.60  1  1    4    2 42.8
mtcars %>% mutate(mpg2 = mpg * 2)
#>                      mpg cyl  disp  hp drat    wt  qsec vs am gear carb mpg2
#> Mazda RX4           21.0   6 160.0 110 3.90 2.620 16.46  0  1    4    4 42.0
#> Mazda RX4 Wag       21.0   6 160.0 110 3.90 2.875 17.02  0  1    4    4 42.0
#> Datsun 710          22.8   4 108.0  93 3.85 2.320 18.61  1  1    4    1 45.6
#> Hornet 4 Drive      21.4   6 258.0 110 3.08 3.215 19.44  1  0    3    1 42.8
#> Hornet Sportabout   18.7   8 360.0 175 3.15 3.440 17.02  0  0    3    2 37.4
#> Valiant             18.1   6 225.0 105 2.76 3.460 20.22  1  0    3    1 36.2
#> Duster 360          14.3   8 360.0 245 3.21 3.570 15.84  0  0    3    4 28.6
#> Merc 240D           24.4   4 146.7  62 3.69 3.190 20.00  1  0    4    2 48.8
#> Merc 230            22.8   4 140.8  95 3.92 3.150 22.90  1  0    4    2 45.6
#> Merc 280            19.2   6 167.6 123 3.92 3.440 18.30  1  0    4    4 38.4
#> Merc 280C           17.8   6 167.6 123 3.92 3.440 18.90  1  0    4    4 35.6
#> Merc 450SE          16.4   8 275.8 180 3.07 4.070 17.40  0  0    3    3 32.8
#> Merc 450SL          17.3   8 275.8 180 3.07 3.730 17.60  0  0    3    3 34.6
#> Merc 450SLC         15.2   8 275.8 180 3.07 3.780 18.00  0  0    3    3 30.4
#> Cadillac Fleetwood  10.4   8 472.0 205 2.93 5.250 17.98  0  0    3    4 20.8
#> Lincoln Continental 10.4   8 460.0 215 3.00 5.424 17.82  0  0    3    4 20.8
#> Chrysler Imperial   14.7   8 440.0 230 3.23 5.345 17.42  0  0    3    4 29.4
#> Fiat 128            32.4   4  78.7  66 4.08 2.200 19.47  1  1    4    1 64.8
#> Honda Civic         30.4   4  75.7  52 4.93 1.615 18.52  1  1    4    2 60.8
#> Toyota Corolla      33.9   4  71.1  65 4.22 1.835 19.90  1  1    4    1 67.8
#> Toyota Corona       21.5   4 120.1  97 3.70 2.465 20.01  1  0    3    1 43.0
#> Dodge Challenger    15.5   8 318.0 150 2.76 3.520 16.87  0  0    3    2 31.0
#> AMC Javelin         15.2   8 304.0 150 3.15 3.435 17.30  0  0    3    2 30.4
#> Camaro Z28          13.3   8 350.0 245 3.73 3.840 15.41  0  0    3    4 26.6
#> Pontiac Firebird    19.2   8 400.0 175 3.08 3.845 17.05  0  0    3    2 38.4
#> Fiat X1-9           27.3   4  79.0  66 4.08 1.935 18.90  1  1    4    1 54.6
#> Porsche 914-2       26.0   4 120.3  91 4.43 2.140 16.70  0  1    5    2 52.0
#> Lotus Europa        30.4   4  95.1 113 3.77 1.513 16.90  1  1    5    2 60.8
#> Ford Pantera L      15.8   8 351.0 264 4.22 3.170 14.50  0  1    5    4 31.6
#> Ferrari Dino        19.7   6 145.0 175 3.62 2.770 15.50  0  1    5    6 39.4
#> Maserati Bora       15.0   8 301.0 335 3.54 3.570 14.60  0  1    5    8 30.0
#> Volvo 142E          21.4   4 121.0 109 4.11 2.780 18.60  1  1    4    2 42.8
mtcars %>% mutate(mpg2 = mpg * 2, cyl2 = cyl * 2)
#>                      mpg cyl  disp  hp drat    wt  qsec vs am gear carb mpg2
#> Mazda RX4           21.0   6 160.0 110 3.90 2.620 16.46  0  1    4    4 42.0
#> Mazda RX4 Wag       21.0   6 160.0 110 3.90 2.875 17.02  0  1    4    4 42.0
#> Datsun 710          22.8   4 108.0  93 3.85 2.320 18.61  1  1    4    1 45.6
#> Hornet 4 Drive      21.4   6 258.0 110 3.08 3.215 19.44  1  0    3    1 42.8
#> Hornet Sportabout   18.7   8 360.0 175 3.15 3.440 17.02  0  0    3    2 37.4
#> Valiant             18.1   6 225.0 105 2.76 3.460 20.22  1  0    3    1 36.2
#> Duster 360          14.3   8 360.0 245 3.21 3.570 15.84  0  0    3    4 28.6
#> Merc 240D           24.4   4 146.7  62 3.69 3.190 20.00  1  0    4    2 48.8
#> Merc 230            22.8   4 140.8  95 3.92 3.150 22.90  1  0    4    2 45.6
#> Merc 280            19.2   6 167.6 123 3.92 3.440 18.30  1  0    4    4 38.4
#> Merc 280C           17.8   6 167.6 123 3.92 3.440 18.90  1  0    4    4 35.6
#> Merc 450SE          16.4   8 275.8 180 3.07 4.070 17.40  0  0    3    3 32.8
#> Merc 450SL          17.3   8 275.8 180 3.07 3.730 17.60  0  0    3    3 34.6
#> Merc 450SLC         15.2   8 275.8 180 3.07 3.780 18.00  0  0    3    3 30.4
#> Cadillac Fleetwood  10.4   8 472.0 205 2.93 5.250 17.98  0  0    3    4 20.8
#> Lincoln Continental 10.4   8 460.0 215 3.00 5.424 17.82  0  0    3    4 20.8
#> Chrysler Imperial   14.7   8 440.0 230 3.23 5.345 17.42  0  0    3    4 29.4
#> Fiat 128            32.4   4  78.7  66 4.08 2.200 19.47  1  1    4    1 64.8
#> Honda Civic         30.4   4  75.7  52 4.93 1.615 18.52  1  1    4    2 60.8
#> Toyota Corolla      33.9   4  71.1  65 4.22 1.835 19.90  1  1    4    1 67.8
#> Toyota Corona       21.5   4 120.1  97 3.70 2.465 20.01  1  0    3    1 43.0
#> Dodge Challenger    15.5   8 318.0 150 2.76 3.520 16.87  0  0    3    2 31.0
#> AMC Javelin         15.2   8 304.0 150 3.15 3.435 17.30  0  0    3    2 30.4
#> Camaro Z28          13.3   8 350.0 245 3.73 3.840 15.41  0  0    3    4 26.6
#> Pontiac Firebird    19.2   8 400.0 175 3.08 3.845 17.05  0  0    3    2 38.4
#> Fiat X1-9           27.3   4  79.0  66 4.08 1.935 18.90  1  1    4    1 54.6
#> Porsche 914-2       26.0   4 120.3  91 4.43 2.140 16.70  0  1    5    2 52.0
#> Lotus Europa        30.4   4  95.1 113 3.77 1.513 16.90  1  1    5    2 60.8
#> Ford Pantera L      15.8   8 351.0 264 4.22 3.170 14.50  0  1    5    4 31.6
#> Ferrari Dino        19.7   6 145.0 175 3.62 2.770 15.50  0  1    5    6 39.4
#> Maserati Bora       15.0   8 301.0 335 3.54 3.570 14.60  0  1    5    8 30.0
#> Volvo 142E          21.4   4 121.0 109 4.11 2.780 18.60  1  1    4    2 42.8
#>                     cyl2
#> Mazda RX4             12
#> Mazda RX4 Wag         12
#> Datsun 710             8
#> Hornet 4 Drive        12
#> Hornet Sportabout     16
#> Valiant               12
#> Duster 360            16
#> Merc 240D              8
#> Merc 230               8
#> Merc 280              12
#> Merc 280C             12
#> Merc 450SE            16
#> Merc 450SL            16
#> Merc 450SLC           16
#> Cadillac Fleetwood    16
#> Lincoln Continental   16
#> Chrysler Imperial     16
#> Fiat 128               8
#> Honda Civic            8
#> Toyota Corolla         8
#> Toyota Corona          8
#> Dodge Challenger      16
#> AMC Javelin           16
#> Camaro Z28            16
#> Pontiac Firebird      16
#> Fiat X1-9              8
#> Porsche 914-2          8
#> Lotus Europa           8
#> Ford Pantera L        16
#> Ferrari Dino          12
#> Maserati Bora         16
#> Volvo 142E             8

# Newly created variables are available immediately
mtcars %>% mutate(mpg2 = mpg * 2, mpg4 = mpg2 * 2)
#>                      mpg cyl  disp  hp drat    wt  qsec vs am gear carb mpg2
#> Mazda RX4           21.0   6 160.0 110 3.90 2.620 16.46  0  1    4    4 42.0
#> Mazda RX4 Wag       21.0   6 160.0 110 3.90 2.875 17.02  0  1    4    4 42.0
#> Datsun 710          22.8   4 108.0  93 3.85 2.320 18.61  1  1    4    1 45.6
#> Hornet 4 Drive      21.4   6 258.0 110 3.08 3.215 19.44  1  0    3    1 42.8
#> Hornet Sportabout   18.7   8 360.0 175 3.15 3.440 17.02  0  0    3    2 37.4
#> Valiant             18.1   6 225.0 105 2.76 3.460 20.22  1  0    3    1 36.2
#> Duster 360          14.3   8 360.0 245 3.21 3.570 15.84  0  0    3    4 28.6
#> Merc 240D           24.4   4 146.7  62 3.69 3.190 20.00  1  0    4    2 48.8
#> Merc 230            22.8   4 140.8  95 3.92 3.150 22.90  1  0    4    2 45.6
#> Merc 280            19.2   6 167.6 123 3.92 3.440 18.30  1  0    4    4 38.4
#> Merc 280C           17.8   6 167.6 123 3.92 3.440 18.90  1  0    4    4 35.6
#> Merc 450SE          16.4   8 275.8 180 3.07 4.070 17.40  0  0    3    3 32.8
#> Merc 450SL          17.3   8 275.8 180 3.07 3.730 17.60  0  0    3    3 34.6
#> Merc 450SLC         15.2   8 275.8 180 3.07 3.780 18.00  0  0    3    3 30.4
#> Cadillac Fleetwood  10.4   8 472.0 205 2.93 5.250 17.98  0  0    3    4 20.8
#> Lincoln Continental 10.4   8 460.0 215 3.00 5.424 17.82  0  0    3    4 20.8
#> Chrysler Imperial   14.7   8 440.0 230 3.23 5.345 17.42  0  0    3    4 29.4
#> Fiat 128            32.4   4  78.7  66 4.08 2.200 19.47  1  1    4    1 64.8
#> Honda Civic         30.4   4  75.7  52 4.93 1.615 18.52  1  1    4    2 60.8
#> Toyota Corolla      33.9   4  71.1  65 4.22 1.835 19.90  1  1    4    1 67.8
#> Toyota Corona       21.5   4 120.1  97 3.70 2.465 20.01  1  0    3    1 43.0
#> Dodge Challenger    15.5   8 318.0 150 2.76 3.520 16.87  0  0    3    2 31.0
#> AMC Javelin         15.2   8 304.0 150 3.15 3.435 17.30  0  0    3    2 30.4
#> Camaro Z28          13.3   8 350.0 245 3.73 3.840 15.41  0  0    3    4 26.6
#> Pontiac Firebird    19.2   8 400.0 175 3.08 3.845 17.05  0  0    3    2 38.4
#> Fiat X1-9           27.3   4  79.0  66 4.08 1.935 18.90  1  1    4    1 54.6
#> Porsche 914-2       26.0   4 120.3  91 4.43 2.140 16.70  0  1    5    2 52.0
#> Lotus Europa        30.4   4  95.1 113 3.77 1.513 16.90  1  1    5    2 60.8
#> Ford Pantera L      15.8   8 351.0 264 4.22 3.170 14.50  0  1    5    4 31.6
#> Ferrari Dino        19.7   6 145.0 175 3.62 2.770 15.50  0  1    5    6 39.4
#> Maserati Bora       15.0   8 301.0 335 3.54 3.570 14.60  0  1    5    8 30.0
#> Volvo 142E          21.4   4 121.0 109 4.11 2.780 18.60  1  1    4    2 42.8
#>                      mpg4
#> Mazda RX4            84.0
#> Mazda RX4 Wag        84.0
#> Datsun 710           91.2
#> Hornet 4 Drive       85.6
#> Hornet Sportabout    74.8
#> Valiant              72.4
#> Duster 360           57.2
#> Merc 240D            97.6
#> Merc 230             91.2
#> Merc 280             76.8
#> Merc 280C            71.2
#> Merc 450SE           65.6
#> Merc 450SL           69.2
#> Merc 450SLC          60.8
#> Cadillac Fleetwood   41.6
#> Lincoln Continental  41.6
#> Chrysler Imperial    58.8
#> Fiat 128            129.6
#> Honda Civic         121.6
#> Toyota Corolla      135.6
#> Toyota Corona        86.0
#> Dodge Challenger     62.0
#> AMC Javelin          60.8
#> Camaro Z28           53.2
#> Pontiac Firebird     76.8
#> Fiat X1-9           109.2
#> Porsche 914-2       104.0
#> Lotus Europa        121.6
#> Ford Pantera L       63.2
#> Ferrari Dino         78.8
#> Maserati Bora        60.0
#> Volvo 142E           85.6

# You can also use mutate() to remove variables and modify existing variables
mtcars %>% mutate(
  mpg = NULL,
  disp = disp * 0.0163871 # convert to litres
)
#>                     cyl     disp  hp drat    wt  qsec vs am gear carb
#> Mazda RX4             6 2.621936 110 3.90 2.620 16.46  0  1    4    4
#> Mazda RX4 Wag         6 2.621936 110 3.90 2.875 17.02  0  1    4    4
#> Datsun 710            4 1.769807  93 3.85 2.320 18.61  1  1    4    1
#> Hornet 4 Drive        6 4.227872 110 3.08 3.215 19.44  1  0    3    1
#> Hornet Sportabout     8 5.899356 175 3.15 3.440 17.02  0  0    3    2
#> Valiant               6 3.687098 105 2.76 3.460 20.22  1  0    3    1
#> Duster 360            8 5.899356 245 3.21 3.570 15.84  0  0    3    4
#> Merc 240D             4 2.403988  62 3.69 3.190 20.00  1  0    4    2
#> Merc 230              4 2.307304  95 3.92 3.150 22.90  1  0    4    2
#> Merc 280              6 2.746478 123 3.92 3.440 18.30  1  0    4    4
#> Merc 280C             6 2.746478 123 3.92 3.440 18.90  1  0    4    4
#> Merc 450SE            8 4.519562 180 3.07 4.070 17.40  0  0    3    3
#> Merc 450SL            8 4.519562 180 3.07 3.730 17.60  0  0    3    3
#> Merc 450SLC           8 4.519562 180 3.07 3.780 18.00  0  0    3    3
#> Cadillac Fleetwood    8 7.734711 205 2.93 5.250 17.98  0  0    3    4
#> Lincoln Continental   8 7.538066 215 3.00 5.424 17.82  0  0    3    4
#> Chrysler Imperial     8 7.210324 230 3.23 5.345 17.42  0  0    3    4
#> Fiat 128              4 1.289665  66 4.08 2.200 19.47  1  1    4    1
#> Honda Civic           4 1.240503  52 4.93 1.615 18.52  1  1    4    2
#> Toyota Corolla        4 1.165123  65 4.22 1.835 19.90  1  1    4    1
#> Toyota Corona         4 1.968091  97 3.70 2.465 20.01  1  0    3    1
#> Dodge Challenger      8 5.211098 150 2.76 3.520 16.87  0  0    3    2
#> AMC Javelin           8 4.981678 150 3.15 3.435 17.30  0  0    3    2
#> Camaro Z28            8 5.735485 245 3.73 3.840 15.41  0  0    3    4
#> Pontiac Firebird      8 6.554840 175 3.08 3.845 17.05  0  0    3    2
#> Fiat X1-9             4 1.294581  66 4.08 1.935 18.90  1  1    4    1
#> Porsche 914-2         4 1.971368  91 4.43 2.140 16.70  0  1    5    2
#> Lotus Europa          4 1.558413 113 3.77 1.513 16.90  1  1    5    2
#> Ford Pantera L        8 5.751872 264 4.22 3.170 14.50  0  1    5    4
#> Ferrari Dino          6 2.376130 175 3.62 2.770 15.50  0  1    5    6
#> Maserati Bora         8 4.932517 335 3.54 3.570 14.60  0  1    5    8
#> Volvo 142E            4 1.982839 109 4.11 2.780 18.60  1  1    4    2

# By default, new columns are placed on the far right.
# You can override this with `.before` or `.after`.
df <- data.frame(x = 1, y = 2)
df %>% mutate(z = x + y)
#>   x y z
#> 1 1 2 3
df %>% mutate(z = x + y, .before = 1)
#>   z x y
#> 1 3 1 2
df %>% mutate(z = x + y, .after = x)
#>   x z y
#> 1 1 3 2

# By default, mutate() keeps all columns from the input data.
# You can override with `.keep`
df <- data.frame(
  x = 1, y = 2, a = "a", b = "b",
  stringsAsFactors = FALSE
)
df %>% mutate(z = x + y, .keep = "all") # the default
#>   x y a b z
#> 1 1 2 a b 3
df %>% mutate(z = x + y, .keep = "used")
#>   x y z
#> 1 1 2 3
df %>% mutate(z = x + y, .keep = "unused")
#>   a b z
#> 1 a b 3
df %>% mutate(z = x + y, .keep = "none") # same as transmute()
#>   z
#> 1 3

# mutate() vs transmute --------------------------
# mutate() keeps all existing variables
mtcars %>%
  mutate(displ_l = disp / 61.0237)
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
#>                      displ_l
#> Mazda RX4           2.621932
#> Mazda RX4 Wag       2.621932
#> Datsun 710          1.769804
#> Hornet 4 Drive      4.227866
#> Hornet Sportabout   5.899347
#> Valiant             3.687092
#> Duster 360          5.899347
#> Merc 240D           2.403984
#> Merc 230            2.307300
#> Merc 280            2.746474
#> Merc 280C           2.746474
#> Merc 450SE          4.519556
#> Merc 450SL          4.519556
#> Merc 450SLC         4.519556
#> Cadillac Fleetwood  7.734700
#> Lincoln Continental 7.538055
#> Chrysler Imperial   7.210313
#> Fiat 128            1.289663
#> Honda Civic         1.240502
#> Toyota Corolla      1.165121
#> Toyota Corona       1.968088
#> Dodge Challenger    5.211090
#> AMC Javelin         4.981671
#> Camaro Z28          5.735477
#> Pontiac Firebird    6.554830
#> Fiat X1-9           1.294579
#> Porsche 914-2       1.971365
#> Lotus Europa        1.558411
#> Ford Pantera L      5.751864
#> Ferrari Dino        2.376126
#> Maserati Bora       4.932510
#> Volvo 142E          1.982836

# transmute keeps only the variables you create
mtcars %>%
  transmute(displ_l = disp / 61.0237)
#>                      displ_l
#> Mazda RX4           2.621932
#> Mazda RX4 Wag       2.621932
#> Datsun 710          1.769804
#> Hornet 4 Drive      4.227866
#> Hornet Sportabout   5.899347
#> Valiant             3.687092
#> Duster 360          5.899347
#> Merc 240D           2.403984
#> Merc 230            2.307300
#> Merc 280            2.746474
#> Merc 280C           2.746474
#> Merc 450SE          4.519556
#> Merc 450SL          4.519556
#> Merc 450SLC         4.519556
#> Cadillac Fleetwood  7.734700
#> Lincoln Continental 7.538055
#> Chrysler Imperial   7.210313
#> Fiat 128            1.289663
#> Honda Civic         1.240502
#> Toyota Corolla      1.165121
#> Toyota Corona       1.968088
#> Dodge Challenger    5.211090
#> AMC Javelin         4.981671
#> Camaro Z28          5.735477
#> Pontiac Firebird    6.554830
#> Fiat X1-9           1.294579
#> Porsche 914-2       1.971365
#> Lotus Europa        1.558411
#> Ford Pantera L      5.751864
#> Ferrari Dino        2.376126
#> Maserati Bora       4.932510
#> Volvo 142E          1.982836
```
