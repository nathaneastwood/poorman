# Efficiently bind multiple `data.frame`s by row and column

Efficiently bind multiple `data.frame`s by row and column

## Usage

``` r
bind_cols(...)

bind_rows(..., .id = NULL)
```

## Arguments

- ...:

  `data.frame`s to combine.

  Each argument can either be a `data.frame`, a `list` that could be a
  `data.frame`, or a `list` of `data.frame`s.

  When row-binding, columns are matched by name, and any missing columns
  will be filled with `NA`.

  When column-binding, rows are matched by position, so all
  `data.frame`s must have the same number of rows. To match by value,
  not position, see [mutate_joins](mutate_joins.md).

- .id:

  `character(1)`. `data.frame` identifier.

  When `.id` is supplied, a new column of identifiers is created to link
  each row to its original `data.frame`. The labels are taken from the
  named arguments to `bind_rows()`. When a `list` of `data.frame`s is
  supplied, the labels are taken from the names of the `list`. If no
  names are found a numeric sequence is used instead.

## Examples

``` r
one <- mtcars[1:4, ]
two <- mtcars[9:12, ]

# You can supply data frames as arguments:
bind_rows(one, two)
#>                 mpg cyl  disp  hp drat    wt  qsec vs am gear carb
#> Mazda RX4      21.0   6 160.0 110 3.90 2.620 16.46  0  1    4    4
#> Mazda RX4 Wag  21.0   6 160.0 110 3.90 2.875 17.02  0  1    4    4
#> Datsun 710     22.8   4 108.0  93 3.85 2.320 18.61  1  1    4    1
#> Hornet 4 Drive 21.4   6 258.0 110 3.08 3.215 19.44  1  0    3    1
#> Merc 230       22.8   4 140.8  95 3.92 3.150 22.90  1  0    4    2
#> Merc 280       19.2   6 167.6 123 3.92 3.440 18.30  1  0    4    4
#> Merc 280C      17.8   6 167.6 123 3.92 3.440 18.90  1  0    4    4
#> Merc 450SE     16.4   8 275.8 180 3.07 4.070 17.40  0  0    3    3

# The contents of lists are spliced automatically:
bind_rows(list(one, two))
#>                 mpg cyl  disp  hp drat    wt  qsec vs am gear carb
#> Mazda RX4      21.0   6 160.0 110 3.90 2.620 16.46  0  1    4    4
#> Mazda RX4 Wag  21.0   6 160.0 110 3.90 2.875 17.02  0  1    4    4
#> Datsun 710     22.8   4 108.0  93 3.85 2.320 18.61  1  1    4    1
#> Hornet 4 Drive 21.4   6 258.0 110 3.08 3.215 19.44  1  0    3    1
#> Merc 230       22.8   4 140.8  95 3.92 3.150 22.90  1  0    4    2
#> Merc 280       19.2   6 167.6 123 3.92 3.440 18.30  1  0    4    4
#> Merc 280C      17.8   6 167.6 123 3.92 3.440 18.90  1  0    4    4
#> Merc 450SE     16.4   8 275.8 180 3.07 4.070 17.40  0  0    3    3
bind_rows(split(mtcars, mtcars$cyl))
#>                      mpg cyl  disp  hp drat    wt  qsec vs am gear carb
#> Datsun 710          22.8   4 108.0  93 3.85 2.320 18.61  1  1    4    1
#> Merc 240D           24.4   4 146.7  62 3.69 3.190 20.00  1  0    4    2
#> Merc 230            22.8   4 140.8  95 3.92 3.150 22.90  1  0    4    2
#> Fiat 128            32.4   4  78.7  66 4.08 2.200 19.47  1  1    4    1
#> Honda Civic         30.4   4  75.7  52 4.93 1.615 18.52  1  1    4    2
#> Toyota Corolla      33.9   4  71.1  65 4.22 1.835 19.90  1  1    4    1
#> Toyota Corona       21.5   4 120.1  97 3.70 2.465 20.01  1  0    3    1
#> Fiat X1-9           27.3   4  79.0  66 4.08 1.935 18.90  1  1    4    1
#> Porsche 914-2       26.0   4 120.3  91 4.43 2.140 16.70  0  1    5    2
#> Lotus Europa        30.4   4  95.1 113 3.77 1.513 16.90  1  1    5    2
#> Volvo 142E          21.4   4 121.0 109 4.11 2.780 18.60  1  1    4    2
#> Mazda RX4           21.0   6 160.0 110 3.90 2.620 16.46  0  1    4    4
#> Mazda RX4 Wag       21.0   6 160.0 110 3.90 2.875 17.02  0  1    4    4
#> Hornet 4 Drive      21.4   6 258.0 110 3.08 3.215 19.44  1  0    3    1
#> Valiant             18.1   6 225.0 105 2.76 3.460 20.22  1  0    3    1
#> Merc 280            19.2   6 167.6 123 3.92 3.440 18.30  1  0    4    4
#> Merc 280C           17.8   6 167.6 123 3.92 3.440 18.90  1  0    4    4
#> Ferrari Dino        19.7   6 145.0 175 3.62 2.770 15.50  0  1    5    6
#> Hornet Sportabout   18.7   8 360.0 175 3.15 3.440 17.02  0  0    3    2
#> Duster 360          14.3   8 360.0 245 3.21 3.570 15.84  0  0    3    4
#> Merc 450SE          16.4   8 275.8 180 3.07 4.070 17.40  0  0    3    3
#> Merc 450SL          17.3   8 275.8 180 3.07 3.730 17.60  0  0    3    3
#> Merc 450SLC         15.2   8 275.8 180 3.07 3.780 18.00  0  0    3    3
#> Cadillac Fleetwood  10.4   8 472.0 205 2.93 5.250 17.98  0  0    3    4
#> Lincoln Continental 10.4   8 460.0 215 3.00 5.424 17.82  0  0    3    4
#> Chrysler Imperial   14.7   8 440.0 230 3.23 5.345 17.42  0  0    3    4
#> Dodge Challenger    15.5   8 318.0 150 2.76 3.520 16.87  0  0    3    2
#> AMC Javelin         15.2   8 304.0 150 3.15 3.435 17.30  0  0    3    2
#> Camaro Z28          13.3   8 350.0 245 3.73 3.840 15.41  0  0    3    4
#> Pontiac Firebird    19.2   8 400.0 175 3.08 3.845 17.05  0  0    3    2
#> Ford Pantera L      15.8   8 351.0 264 4.22 3.170 14.50  0  1    5    4
#> Maserati Bora       15.0   8 301.0 335 3.54 3.570 14.60  0  1    5    8
bind_rows(list(one, two), list(two, one))
#>                  mpg cyl  disp  hp drat    wt  qsec vs am gear carb
#> Mazda RX4       21.0   6 160.0 110 3.90 2.620 16.46  0  1    4    4
#> Mazda RX4 Wag   21.0   6 160.0 110 3.90 2.875 17.02  0  1    4    4
#> Datsun 710      22.8   4 108.0  93 3.85 2.320 18.61  1  1    4    1
#> Hornet 4 Drive  21.4   6 258.0 110 3.08 3.215 19.44  1  0    3    1
#> Merc 230        22.8   4 140.8  95 3.92 3.150 22.90  1  0    4    2
#> Merc 280        19.2   6 167.6 123 3.92 3.440 18.30  1  0    4    4
#> Merc 280C       17.8   6 167.6 123 3.92 3.440 18.90  1  0    4    4
#> Merc 450SE      16.4   8 275.8 180 3.07 4.070 17.40  0  0    3    3
#> Merc 2301       22.8   4 140.8  95 3.92 3.150 22.90  1  0    4    2
#> Merc 2801       19.2   6 167.6 123 3.92 3.440 18.30  1  0    4    4
#> Merc 280C1      17.8   6 167.6 123 3.92 3.440 18.90  1  0    4    4
#> Merc 450SE1     16.4   8 275.8 180 3.07 4.070 17.40  0  0    3    3
#> Mazda RX41      21.0   6 160.0 110 3.90 2.620 16.46  0  1    4    4
#> Mazda RX4 Wag1  21.0   6 160.0 110 3.90 2.875 17.02  0  1    4    4
#> Datsun 7101     22.8   4 108.0  93 3.85 2.320 18.61  1  1    4    1
#> Hornet 4 Drive1 21.4   6 258.0 110 3.08 3.215 19.44  1  0    3    1

# In addition to data frames, you can supply vectors. In the rows
# direction, the vectors represent rows and should have inner
# names:
bind_rows(
  c(a = 1, b = 2),
  c(a = 3, b = 4)
)
#>   a b
#> 1 1 2
#> 2 3 4

# You can mix vectors and data frames:
bind_rows(
  c(a = 1, b = 2),
  data.frame(a = 3:4, b = 5:6),
  c(a = 7, b = 8)
)
#>   a b
#> 1 1 2
#> 2 3 5
#> 3 4 6
#> 4 7 8

# When you supply a column name with the `.id` argument, a new
# column is created to link each row to its original data frame
bind_rows(list(one, two), .id = "id")
#>                id  mpg cyl  disp  hp drat    wt  qsec vs am gear carb
#> Mazda RX4       1 21.0   6 160.0 110 3.90 2.620 16.46  0  1    4    4
#> Mazda RX4 Wag   1 21.0   6 160.0 110 3.90 2.875 17.02  0  1    4    4
#> Datsun 710      1 22.8   4 108.0  93 3.85 2.320 18.61  1  1    4    1
#> Hornet 4 Drive  1 21.4   6 258.0 110 3.08 3.215 19.44  1  0    3    1
#> Merc 230        2 22.8   4 140.8  95 3.92 3.150 22.90  1  0    4    2
#> Merc 280        2 19.2   6 167.6 123 3.92 3.440 18.30  1  0    4    4
#> Merc 280C       2 17.8   6 167.6 123 3.92 3.440 18.90  1  0    4    4
#> Merc 450SE      2 16.4   8 275.8 180 3.07 4.070 17.40  0  0    3    3
bind_rows(list(a = one, b = two), .id = "id")
#>                id  mpg cyl  disp  hp drat    wt  qsec vs am gear carb
#> Mazda RX4       a 21.0   6 160.0 110 3.90 2.620 16.46  0  1    4    4
#> Mazda RX4 Wag   a 21.0   6 160.0 110 3.90 2.875 17.02  0  1    4    4
#> Datsun 710      a 22.8   4 108.0  93 3.85 2.320 18.61  1  1    4    1
#> Hornet 4 Drive  a 21.4   6 258.0 110 3.08 3.215 19.44  1  0    3    1
#> Merc 230        b 22.8   4 140.8  95 3.92 3.150 22.90  1  0    4    2
#> Merc 280        b 19.2   6 167.6 123 3.92 3.440 18.30  1  0    4    4
#> Merc 280C       b 17.8   6 167.6 123 3.92 3.440 18.90  1  0    4    4
#> Merc 450SE      b 16.4   8 275.8 180 3.07 4.070 17.40  0  0    3    3
bind_rows("group 1" = one, "group 2" = two, .id = "groups")
#>                 groups  mpg cyl  disp  hp drat    wt  qsec vs am gear carb
#> Mazda RX4      group 1 21.0   6 160.0 110 3.90 2.620 16.46  0  1    4    4
#> Mazda RX4 Wag  group 1 21.0   6 160.0 110 3.90 2.875 17.02  0  1    4    4
#> Datsun 710     group 1 22.8   4 108.0  93 3.85 2.320 18.61  1  1    4    1
#> Hornet 4 Drive group 1 21.4   6 258.0 110 3.08 3.215 19.44  1  0    3    1
#> Merc 230       group 2 22.8   4 140.8  95 3.92 3.150 22.90  1  0    4    2
#> Merc 280       group 2 19.2   6 167.6 123 3.92 3.440 18.30  1  0    4    4
#> Merc 280C      group 2 17.8   6 167.6 123 3.92 3.440 18.90  1  0    4    4
#> Merc 450SE     group 2 16.4   8 275.8 180 3.07 4.070 17.40  0  0    3    3

if (FALSE) { # \dontrun{
# Rows need to match when column-binding
bind_cols(data.frame(x = 1:3), data.frame(y = 1:2))

# even with 0 columns
bind_cols(data.frame(x = 1:3), data.frame())
} # }

bind_cols(one, two)
#>                 mpg cyl disp  hp drat    wt  qsec vs am gear carb  mpg cyl
#> Mazda RX4      21.0   6  160 110 3.90 2.620 16.46  0  1    4    4 22.8   4
#> Mazda RX4 Wag  21.0   6  160 110 3.90 2.875 17.02  0  1    4    4 19.2   6
#> Datsun 710     22.8   4  108  93 3.85 2.320 18.61  1  1    4    1 17.8   6
#> Hornet 4 Drive 21.4   6  258 110 3.08 3.215 19.44  1  0    3    1 16.4   8
#>                 disp  hp drat   wt qsec vs am gear carb
#> Mazda RX4      140.8  95 3.92 3.15 22.9  1  0    4    2
#> Mazda RX4 Wag  167.6 123 3.92 3.44 18.3  1  0    4    4
#> Datsun 710     167.6 123 3.92 3.44 18.9  1  0    4    4
#> Hornet 4 Drive 275.8 180 3.07 4.07 17.4  0  0    3    3
bind_cols(list(one, two))
#>                 mpg cyl disp  hp drat    wt  qsec vs am gear carb  mpg cyl
#> Mazda RX4      21.0   6  160 110 3.90 2.620 16.46  0  1    4    4 22.8   4
#> Mazda RX4 Wag  21.0   6  160 110 3.90 2.875 17.02  0  1    4    4 19.2   6
#> Datsun 710     22.8   4  108  93 3.85 2.320 18.61  1  1    4    1 17.8   6
#> Hornet 4 Drive 21.4   6  258 110 3.08 3.215 19.44  1  0    3    1 16.4   8
#>                 disp  hp drat   wt qsec vs am gear carb
#> Mazda RX4      140.8  95 3.92 3.15 22.9  1  0    4    2
#> Mazda RX4 Wag  167.6 123 3.92 3.44 18.3  1  0    4    4
#> Datsun 710     167.6 123 3.92 3.44 18.9  1  0    4    4
#> Hornet 4 Drive 275.8 180 3.07 4.07 17.4  0  0    3    3
```
