# Group by one or more variables

Determine the groups within a `data.frame` to perform operations on.
`ungroup()` removes the grouping levels.

## Usage

``` r
group_by(.data, ..., .add = FALSE, .drop = group_by_drop_default(.data))

ungroup(x, ...)
```

## Arguments

- .data:

  `data.frame`. The data to group.

- ...:

  One or more unquoted column names to group/ungroup the data by.

- .add:

  `logical(1)`. When `FALSE` (the default) `group_by()` will override
  existing groups. To add to existing groups, use `.add = TRUE`.

- .drop:

  `logical(1)`. Drop groups formed by factor levels that don't appear in
  the data? The default is `TRUE` except when `.data` has been
  previously grouped with `.drop = FALSE`. See
  [`group_by_drop_default()`](group_by_drop_default.md) for details.

- x:

  A `data.frame`.

## Value

When using `group_by()`, a `data.frame`, grouped by the grouping
variables.

When using `ungroup()`, a `data.frame`.

## Examples

``` r
group_by(mtcars, am, cyl)
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
ungroup(mutate(group_by(mtcars, am, cyl), sumMpg = sum(mpg)))
#>                      mpg cyl  disp  hp drat    wt  qsec vs am gear carb sumMpg
#> Mazda RX4           21.0   6 160.0 110 3.90 2.620 16.46  0  1    4    4   61.7
#> Mazda RX4 Wag       21.0   6 160.0 110 3.90 2.875 17.02  0  1    4    4   61.7
#> Datsun 710          22.8   4 108.0  93 3.85 2.320 18.61  1  1    4    1  224.6
#> Hornet 4 Drive      21.4   6 258.0 110 3.08 3.215 19.44  1  0    3    1   76.5
#> Hornet Sportabout   18.7   8 360.0 175 3.15 3.440 17.02  0  0    3    2  180.6
#> Valiant             18.1   6 225.0 105 2.76 3.460 20.22  1  0    3    1   76.5
#> Duster 360          14.3   8 360.0 245 3.21 3.570 15.84  0  0    3    4  180.6
#> Merc 240D           24.4   4 146.7  62 3.69 3.190 20.00  1  0    4    2   68.7
#> Merc 230            22.8   4 140.8  95 3.92 3.150 22.90  1  0    4    2   68.7
#> Merc 280            19.2   6 167.6 123 3.92 3.440 18.30  1  0    4    4   76.5
#> Merc 280C           17.8   6 167.6 123 3.92 3.440 18.90  1  0    4    4   76.5
#> Merc 450SE          16.4   8 275.8 180 3.07 4.070 17.40  0  0    3    3  180.6
#> Merc 450SL          17.3   8 275.8 180 3.07 3.730 17.60  0  0    3    3  180.6
#> Merc 450SLC         15.2   8 275.8 180 3.07 3.780 18.00  0  0    3    3  180.6
#> Cadillac Fleetwood  10.4   8 472.0 205 2.93 5.250 17.98  0  0    3    4  180.6
#> Lincoln Continental 10.4   8 460.0 215 3.00 5.424 17.82  0  0    3    4  180.6
#> Chrysler Imperial   14.7   8 440.0 230 3.23 5.345 17.42  0  0    3    4  180.6
#> Fiat 128            32.4   4  78.7  66 4.08 2.200 19.47  1  1    4    1  224.6
#> Honda Civic         30.4   4  75.7  52 4.93 1.615 18.52  1  1    4    2  224.6
#> Toyota Corolla      33.9   4  71.1  65 4.22 1.835 19.90  1  1    4    1  224.6
#> Toyota Corona       21.5   4 120.1  97 3.70 2.465 20.01  1  0    3    1   68.7
#> Dodge Challenger    15.5   8 318.0 150 2.76 3.520 16.87  0  0    3    2  180.6
#> AMC Javelin         15.2   8 304.0 150 3.15 3.435 17.30  0  0    3    2  180.6
#> Camaro Z28          13.3   8 350.0 245 3.73 3.840 15.41  0  0    3    4  180.6
#> Pontiac Firebird    19.2   8 400.0 175 3.08 3.845 17.05  0  0    3    2  180.6
#> Fiat X1-9           27.3   4  79.0  66 4.08 1.935 18.90  1  1    4    1  224.6
#> Porsche 914-2       26.0   4 120.3  91 4.43 2.140 16.70  0  1    5    2  224.6
#> Lotus Europa        30.4   4  95.1 113 3.77 1.513 16.90  1  1    5    2  224.6
#> Ford Pantera L      15.8   8 351.0 264 4.22 3.170 14.50  0  1    5    4   30.8
#> Ferrari Dino        19.7   6 145.0 175 3.62 2.770 15.50  0  1    5    6   61.7
#> Maserati Bora       15.0   8 301.0 335 3.54 3.570 14.60  0  1    5    8   30.8
#> Volvo 142E          21.4   4 121.0 109 4.11 2.780 18.60  1  1    4    2  224.6
mtcars %>%
  group_by(am, cyl) %>%
  mutate(sumMpg = sum(mpg)) %>%
  ungroup()
#>                      mpg cyl  disp  hp drat    wt  qsec vs am gear carb sumMpg
#> Mazda RX4           21.0   6 160.0 110 3.90 2.620 16.46  0  1    4    4   61.7
#> Mazda RX4 Wag       21.0   6 160.0 110 3.90 2.875 17.02  0  1    4    4   61.7
#> Datsun 710          22.8   4 108.0  93 3.85 2.320 18.61  1  1    4    1  224.6
#> Hornet 4 Drive      21.4   6 258.0 110 3.08 3.215 19.44  1  0    3    1   76.5
#> Hornet Sportabout   18.7   8 360.0 175 3.15 3.440 17.02  0  0    3    2  180.6
#> Valiant             18.1   6 225.0 105 2.76 3.460 20.22  1  0    3    1   76.5
#> Duster 360          14.3   8 360.0 245 3.21 3.570 15.84  0  0    3    4  180.6
#> Merc 240D           24.4   4 146.7  62 3.69 3.190 20.00  1  0    4    2   68.7
#> Merc 230            22.8   4 140.8  95 3.92 3.150 22.90  1  0    4    2   68.7
#> Merc 280            19.2   6 167.6 123 3.92 3.440 18.30  1  0    4    4   76.5
#> Merc 280C           17.8   6 167.6 123 3.92 3.440 18.90  1  0    4    4   76.5
#> Merc 450SE          16.4   8 275.8 180 3.07 4.070 17.40  0  0    3    3  180.6
#> Merc 450SL          17.3   8 275.8 180 3.07 3.730 17.60  0  0    3    3  180.6
#> Merc 450SLC         15.2   8 275.8 180 3.07 3.780 18.00  0  0    3    3  180.6
#> Cadillac Fleetwood  10.4   8 472.0 205 2.93 5.250 17.98  0  0    3    4  180.6
#> Lincoln Continental 10.4   8 460.0 215 3.00 5.424 17.82  0  0    3    4  180.6
#> Chrysler Imperial   14.7   8 440.0 230 3.23 5.345 17.42  0  0    3    4  180.6
#> Fiat 128            32.4   4  78.7  66 4.08 2.200 19.47  1  1    4    1  224.6
#> Honda Civic         30.4   4  75.7  52 4.93 1.615 18.52  1  1    4    2  224.6
#> Toyota Corolla      33.9   4  71.1  65 4.22 1.835 19.90  1  1    4    1  224.6
#> Toyota Corona       21.5   4 120.1  97 3.70 2.465 20.01  1  0    3    1   68.7
#> Dodge Challenger    15.5   8 318.0 150 2.76 3.520 16.87  0  0    3    2  180.6
#> AMC Javelin         15.2   8 304.0 150 3.15 3.435 17.30  0  0    3    2  180.6
#> Camaro Z28          13.3   8 350.0 245 3.73 3.840 15.41  0  0    3    4  180.6
#> Pontiac Firebird    19.2   8 400.0 175 3.08 3.845 17.05  0  0    3    2  180.6
#> Fiat X1-9           27.3   4  79.0  66 4.08 1.935 18.90  1  1    4    1  224.6
#> Porsche 914-2       26.0   4 120.3  91 4.43 2.140 16.70  0  1    5    2  224.6
#> Lotus Europa        30.4   4  95.1 113 3.77 1.513 16.90  1  1    5    2  224.6
#> Ford Pantera L      15.8   8 351.0 264 4.22 3.170 14.50  0  1    5    4   30.8
#> Ferrari Dino        19.7   6 145.0 175 3.62 2.770 15.50  0  1    5    6   61.7
#> Maserati Bora       15.0   8 301.0 335 3.54 3.570 14.60  0  1    5    8   30.8
#> Volvo 142E          21.4   4 121.0 109 4.11 2.780 18.60  1  1    4    2  224.6
mtcars %>%
  group_by(carb) %>%
  filter(any(gear == 5))
#>                      mpg cyl  disp  hp drat    wt  qsec vs am gear carb
#> Mazda RX4           21.0   6 160.0 110 3.90 2.620 16.46  0  1    4    4
#> Mazda RX4 Wag       21.0   6 160.0 110 3.90 2.875 17.02  0  1    4    4
#> Hornet Sportabout   18.7   8 360.0 175 3.15 3.440 17.02  0  0    3    2
#> Duster 360          14.3   8 360.0 245 3.21 3.570 15.84  0  0    3    4
#> Merc 240D           24.4   4 146.7  62 3.69 3.190 20.00  1  0    4    2
#> Merc 230            22.8   4 140.8  95 3.92 3.150 22.90  1  0    4    2
#> Merc 280            19.2   6 167.6 123 3.92 3.440 18.30  1  0    4    4
#> Merc 280C           17.8   6 167.6 123 3.92 3.440 18.90  1  0    4    4
#> Cadillac Fleetwood  10.4   8 472.0 205 2.93 5.250 17.98  0  0    3    4
#> Lincoln Continental 10.4   8 460.0 215 3.00 5.424 17.82  0  0    3    4
#> Chrysler Imperial   14.7   8 440.0 230 3.23 5.345 17.42  0  0    3    4
#> Honda Civic         30.4   4  75.7  52 4.93 1.615 18.52  1  1    4    2
#> Dodge Challenger    15.5   8 318.0 150 2.76 3.520 16.87  0  0    3    2
#> AMC Javelin         15.2   8 304.0 150 3.15 3.435 17.30  0  0    3    2
#> Camaro Z28          13.3   8 350.0 245 3.73 3.840 15.41  0  0    3    4
#> Pontiac Firebird    19.2   8 400.0 175 3.08 3.845 17.05  0  0    3    2
#> Porsche 914-2       26.0   4 120.3  91 4.43 2.140 16.70  0  1    5    2
#> Lotus Europa        30.4   4  95.1 113 3.77 1.513 16.90  1  1    5    2
#> Ford Pantera L      15.8   8 351.0 264 4.22 3.170 14.50  0  1    5    4
#> Ferrari Dino        19.7   6 145.0 175 3.62 2.770 15.50  0  1    5    6
#> Maserati Bora       15.0   8 301.0 335 3.54 3.570 14.60  0  1    5    8
#> Volvo 142E          21.4   4 121.0 109 4.11 2.780 18.60  1  1    4    2

# You can group by expressions: this is just short-hand for
# a mutate() followed by a group_by()
mtcars %>% group_by(vsam = vs + am)
#>                      mpg cyl  disp  hp drat    wt  qsec vs am gear carb vsam
#> Mazda RX4           21.0   6 160.0 110 3.90 2.620 16.46  0  1    4    4    1
#> Mazda RX4 Wag       21.0   6 160.0 110 3.90 2.875 17.02  0  1    4    4    1
#> Datsun 710          22.8   4 108.0  93 3.85 2.320 18.61  1  1    4    1    2
#> Hornet 4 Drive      21.4   6 258.0 110 3.08 3.215 19.44  1  0    3    1    1
#> Hornet Sportabout   18.7   8 360.0 175 3.15 3.440 17.02  0  0    3    2    0
#> Valiant             18.1   6 225.0 105 2.76 3.460 20.22  1  0    3    1    1
#> Duster 360          14.3   8 360.0 245 3.21 3.570 15.84  0  0    3    4    0
#> Merc 240D           24.4   4 146.7  62 3.69 3.190 20.00  1  0    4    2    1
#> Merc 230            22.8   4 140.8  95 3.92 3.150 22.90  1  0    4    2    1
#> Merc 280            19.2   6 167.6 123 3.92 3.440 18.30  1  0    4    4    1
#> Merc 280C           17.8   6 167.6 123 3.92 3.440 18.90  1  0    4    4    1
#> Merc 450SE          16.4   8 275.8 180 3.07 4.070 17.40  0  0    3    3    0
#> Merc 450SL          17.3   8 275.8 180 3.07 3.730 17.60  0  0    3    3    0
#> Merc 450SLC         15.2   8 275.8 180 3.07 3.780 18.00  0  0    3    3    0
#> Cadillac Fleetwood  10.4   8 472.0 205 2.93 5.250 17.98  0  0    3    4    0
#> Lincoln Continental 10.4   8 460.0 215 3.00 5.424 17.82  0  0    3    4    0
#> Chrysler Imperial   14.7   8 440.0 230 3.23 5.345 17.42  0  0    3    4    0
#> Fiat 128            32.4   4  78.7  66 4.08 2.200 19.47  1  1    4    1    2
#> Honda Civic         30.4   4  75.7  52 4.93 1.615 18.52  1  1    4    2    2
#> Toyota Corolla      33.9   4  71.1  65 4.22 1.835 19.90  1  1    4    1    2
#> Toyota Corona       21.5   4 120.1  97 3.70 2.465 20.01  1  0    3    1    1
#> Dodge Challenger    15.5   8 318.0 150 2.76 3.520 16.87  0  0    3    2    0
#> AMC Javelin         15.2   8 304.0 150 3.15 3.435 17.30  0  0    3    2    0
#> Camaro Z28          13.3   8 350.0 245 3.73 3.840 15.41  0  0    3    4    0
#> Pontiac Firebird    19.2   8 400.0 175 3.08 3.845 17.05  0  0    3    2    0
#> Fiat X1-9           27.3   4  79.0  66 4.08 1.935 18.90  1  1    4    1    2
#> Porsche 914-2       26.0   4 120.3  91 4.43 2.140 16.70  0  1    5    2    1
#> Lotus Europa        30.4   4  95.1 113 3.77 1.513 16.90  1  1    5    2    2
#> Ford Pantera L      15.8   8 351.0 264 4.22 3.170 14.50  0  1    5    4    1
#> Ferrari Dino        19.7   6 145.0 175 3.62 2.770 15.50  0  1    5    6    1
#> Maserati Bora       15.0   8 301.0 335 3.54 3.570 14.60  0  1    5    8    1
#> Volvo 142E          21.4   4 121.0 109 4.11 2.780 18.60  1  1    4    2    2
```
