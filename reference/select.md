# Subset columns using their names and types

Select (and optionally rename) variables in a `data.frame`, using a
concise mini-language that makes it easy to refer to variables based on
their name (e.g. `a:f` selects all columns from `a` on the left to `f`
on the right). You can also use predicate functions like
[`is.numeric()`](https://rdrr.io/r/base/numeric.html) to select
variables based on their properties.

## Usage

``` r
select(.data, ...)
```

## Arguments

- .data:

  A `data.frame`.

- ...:

  \<[`poor-select`](select_helpers.md)\> One or more unquoted
  expressions separated by commas. Variable names can be used as if they
  were positions in the data frame, so expressions like `x:y` can be
  used to select a range of variables.

## Value

An object of the same type as `.data`. The output has the following
properties:

- Rows are not affected.

- Output columns are a subset of input columns, potentially with a
  different order. Columns will be renamed if `new_name = old_name` form
  is used.

- Data frame attributes are preserved.

- Groups are maintained; you can't select off grouping variables.

## Details

### Overview of selection features

poorman selections implement a dialect of R where operators make it easy
to select variables:

- `:` for selecting a range of consecutive variables.

- `!` for taking the complement of a set of variables.

- `&` and `|` for selecting the intersection or the union of two sets of
  variables.

- [`c()`](https://rdrr.io/r/base/c.html) for combining selections.

In addition, you can use **selection helpers**. Some helpers select
specific columns:

- [`everything()`](select_helpers.md): Matches all variables.

- [`last_col()`](select_helpers.md): Select last variable, possibly with
  an offset.

These helpers select variables by matching patterns in their names:

- [`starts_with()`](select_helpers.md): Starts with a prefix.

- [`ends_with()`](select_helpers.md): Ends with a suffix.

- [`contains()`](select_helpers.md): Contains a literal string.

- [`matches()`](select_helpers.md): Matches a regular expression.

- [`num_range()`](select_helpers.md): Matches a numerical range like
  `x01`, `x02`, `x03`.

These helpers select variables from a character vector:

- [`all_of()`](select_helpers.md): Matches variable names in a character
  vector. All names must be present, otherwise an out-of-bounds error is
  thrown.

- [`any_of()`](select_helpers.md): Same as
  [`all_of()`](select_helpers.md), except that no error is thrown for
  names that don't exist.

This helper selects variables with a function:

- [`where()`](where.md): Applies a function to all variables and selects
  those for which the function returns `TRUE`.

## Examples

``` r
# Here we show the usage for the basic selection operators. See the
# specific help pages to learn about helpers like [starts_with()].

# Select variables by name:
mtcars %>% select(mpg)
#>                      mpg
#> Mazda RX4           21.0
#> Mazda RX4 Wag       21.0
#> Datsun 710          22.8
#> Hornet 4 Drive      21.4
#> Hornet Sportabout   18.7
#> Valiant             18.1
#> Duster 360          14.3
#> Merc 240D           24.4
#> Merc 230            22.8
#> Merc 280            19.2
#> Merc 280C           17.8
#> Merc 450SE          16.4
#> Merc 450SL          17.3
#> Merc 450SLC         15.2
#> Cadillac Fleetwood  10.4
#> Lincoln Continental 10.4
#> Chrysler Imperial   14.7
#> Fiat 128            32.4
#> Honda Civic         30.4
#> Toyota Corolla      33.9
#> Toyota Corona       21.5
#> Dodge Challenger    15.5
#> AMC Javelin         15.2
#> Camaro Z28          13.3
#> Pontiac Firebird    19.2
#> Fiat X1-9           27.3
#> Porsche 914-2       26.0
#> Lotus Europa        30.4
#> Ford Pantera L      15.8
#> Ferrari Dino        19.7
#> Maserati Bora       15.0
#> Volvo 142E          21.4

# Select multiple variables by separating them with commas. Note
# how the order of columns is determined by the order of inputs:
mtcars %>% select(disp, gear, am)
#>                      disp gear am
#> Mazda RX4           160.0    4  1
#> Mazda RX4 Wag       160.0    4  1
#> Datsun 710          108.0    4  1
#> Hornet 4 Drive      258.0    3  0
#> Hornet Sportabout   360.0    3  0
#> Valiant             225.0    3  0
#> Duster 360          360.0    3  0
#> Merc 240D           146.7    4  0
#> Merc 230            140.8    4  0
#> Merc 280            167.6    4  0
#> Merc 280C           167.6    4  0
#> Merc 450SE          275.8    3  0
#> Merc 450SL          275.8    3  0
#> Merc 450SLC         275.8    3  0
#> Cadillac Fleetwood  472.0    3  0
#> Lincoln Continental 460.0    3  0
#> Chrysler Imperial   440.0    3  0
#> Fiat 128             78.7    4  1
#> Honda Civic          75.7    4  1
#> Toyota Corolla       71.1    4  1
#> Toyota Corona       120.1    3  0
#> Dodge Challenger    318.0    3  0
#> AMC Javelin         304.0    3  0
#> Camaro Z28          350.0    3  0
#> Pontiac Firebird    400.0    3  0
#> Fiat X1-9            79.0    4  1
#> Porsche 914-2       120.3    5  1
#> Lotus Europa         95.1    5  1
#> Ford Pantera L      351.0    5  1
#> Ferrari Dino        145.0    5  1
#> Maserati Bora       301.0    5  1
#> Volvo 142E          121.0    4  1

# Rename variables:
mtcars %>% select(MilesPerGallon = mpg, everything())
#>                     MilesPerGallon cyl  disp  hp drat    wt  qsec vs am gear
#> Mazda RX4                     21.0   6 160.0 110 3.90 2.620 16.46  0  1    4
#> Mazda RX4 Wag                 21.0   6 160.0 110 3.90 2.875 17.02  0  1    4
#> Datsun 710                    22.8   4 108.0  93 3.85 2.320 18.61  1  1    4
#> Hornet 4 Drive                21.4   6 258.0 110 3.08 3.215 19.44  1  0    3
#> Hornet Sportabout             18.7   8 360.0 175 3.15 3.440 17.02  0  0    3
#> Valiant                       18.1   6 225.0 105 2.76 3.460 20.22  1  0    3
#> Duster 360                    14.3   8 360.0 245 3.21 3.570 15.84  0  0    3
#> Merc 240D                     24.4   4 146.7  62 3.69 3.190 20.00  1  0    4
#> Merc 230                      22.8   4 140.8  95 3.92 3.150 22.90  1  0    4
#> Merc 280                      19.2   6 167.6 123 3.92 3.440 18.30  1  0    4
#> Merc 280C                     17.8   6 167.6 123 3.92 3.440 18.90  1  0    4
#> Merc 450SE                    16.4   8 275.8 180 3.07 4.070 17.40  0  0    3
#> Merc 450SL                    17.3   8 275.8 180 3.07 3.730 17.60  0  0    3
#> Merc 450SLC                   15.2   8 275.8 180 3.07 3.780 18.00  0  0    3
#> Cadillac Fleetwood            10.4   8 472.0 205 2.93 5.250 17.98  0  0    3
#> Lincoln Continental           10.4   8 460.0 215 3.00 5.424 17.82  0  0    3
#> Chrysler Imperial             14.7   8 440.0 230 3.23 5.345 17.42  0  0    3
#> Fiat 128                      32.4   4  78.7  66 4.08 2.200 19.47  1  1    4
#> Honda Civic                   30.4   4  75.7  52 4.93 1.615 18.52  1  1    4
#> Toyota Corolla                33.9   4  71.1  65 4.22 1.835 19.90  1  1    4
#> Toyota Corona                 21.5   4 120.1  97 3.70 2.465 20.01  1  0    3
#> Dodge Challenger              15.5   8 318.0 150 2.76 3.520 16.87  0  0    3
#> AMC Javelin                   15.2   8 304.0 150 3.15 3.435 17.30  0  0    3
#> Camaro Z28                    13.3   8 350.0 245 3.73 3.840 15.41  0  0    3
#> Pontiac Firebird              19.2   8 400.0 175 3.08 3.845 17.05  0  0    3
#> Fiat X1-9                     27.3   4  79.0  66 4.08 1.935 18.90  1  1    4
#> Porsche 914-2                 26.0   4 120.3  91 4.43 2.140 16.70  0  1    5
#> Lotus Europa                  30.4   4  95.1 113 3.77 1.513 16.90  1  1    5
#> Ford Pantera L                15.8   8 351.0 264 4.22 3.170 14.50  0  1    5
#> Ferrari Dino                  19.7   6 145.0 175 3.62 2.770 15.50  0  1    5
#> Maserati Bora                 15.0   8 301.0 335 3.54 3.570 14.60  0  1    5
#> Volvo 142E                    21.4   4 121.0 109 4.11 2.780 18.60  1  1    4
#>                     carb
#> Mazda RX4              4
#> Mazda RX4 Wag          4
#> Datsun 710             1
#> Hornet 4 Drive         1
#> Hornet Sportabout      2
#> Valiant                1
#> Duster 360             4
#> Merc 240D              2
#> Merc 230               2
#> Merc 280               4
#> Merc 280C              4
#> Merc 450SE             3
#> Merc 450SL             3
#> Merc 450SLC            3
#> Cadillac Fleetwood     4
#> Lincoln Continental    4
#> Chrysler Imperial      4
#> Fiat 128               1
#> Honda Civic            2
#> Toyota Corolla         1
#> Toyota Corona          1
#> Dodge Challenger       2
#> AMC Javelin            2
#> Camaro Z28             4
#> Pontiac Firebird       2
#> Fiat X1-9              1
#> Porsche 914-2          2
#> Lotus Europa           2
#> Ford Pantera L         4
#> Ferrari Dino           6
#> Maserati Bora          8
#> Volvo 142E             2

# The `:` operator selects a range of consecutive variables:
select(mtcars, mpg:cyl)
#>                      mpg cyl
#> Mazda RX4           21.0   6
#> Mazda RX4 Wag       21.0   6
#> Datsun 710          22.8   4
#> Hornet 4 Drive      21.4   6
#> Hornet Sportabout   18.7   8
#> Valiant             18.1   6
#> Duster 360          14.3   8
#> Merc 240D           24.4   4
#> Merc 230            22.8   4
#> Merc 280            19.2   6
#> Merc 280C           17.8   6
#> Merc 450SE          16.4   8
#> Merc 450SL          17.3   8
#> Merc 450SLC         15.2   8
#> Cadillac Fleetwood  10.4   8
#> Lincoln Continental 10.4   8
#> Chrysler Imperial   14.7   8
#> Fiat 128            32.4   4
#> Honda Civic         30.4   4
#> Toyota Corolla      33.9   4
#> Toyota Corona       21.5   4
#> Dodge Challenger    15.5   8
#> AMC Javelin         15.2   8
#> Camaro Z28          13.3   8
#> Pontiac Firebird    19.2   8
#> Fiat X1-9           27.3   4
#> Porsche 914-2       26.0   4
#> Lotus Europa        30.4   4
#> Ford Pantera L      15.8   8
#> Ferrari Dino        19.7   6
#> Maserati Bora       15.0   8
#> Volvo 142E          21.4   4

# The `!` operator negates a selection:
mtcars %>% select(!(mpg:qsec))
#>                     vs am gear carb
#> Mazda RX4            0  1    4    4
#> Mazda RX4 Wag        0  1    4    4
#> Datsun 710           1  1    4    1
#> Hornet 4 Drive       1  0    3    1
#> Hornet Sportabout    0  0    3    2
#> Valiant              1  0    3    1
#> Duster 360           0  0    3    4
#> Merc 240D            1  0    4    2
#> Merc 230             1  0    4    2
#> Merc 280             1  0    4    4
#> Merc 280C            1  0    4    4
#> Merc 450SE           0  0    3    3
#> Merc 450SL           0  0    3    3
#> Merc 450SLC          0  0    3    3
#> Cadillac Fleetwood   0  0    3    4
#> Lincoln Continental  0  0    3    4
#> Chrysler Imperial    0  0    3    4
#> Fiat 128             1  1    4    1
#> Honda Civic          1  1    4    2
#> Toyota Corolla       1  1    4    1
#> Toyota Corona        1  0    3    1
#> Dodge Challenger     0  0    3    2
#> AMC Javelin          0  0    3    2
#> Camaro Z28           0  0    3    4
#> Pontiac Firebird     0  0    3    2
#> Fiat X1-9            1  1    4    1
#> Porsche 914-2        0  1    5    2
#> Lotus Europa         1  1    5    2
#> Ford Pantera L       0  1    5    4
#> Ferrari Dino         0  1    5    6
#> Maserati Bora        0  1    5    8
#> Volvo 142E           1  1    4    2
mtcars %>% select(!ends_with("p"))
#>                      mpg cyl drat    wt  qsec vs am gear carb
#> Mazda RX4           21.0   6 3.90 2.620 16.46  0  1    4    4
#> Mazda RX4 Wag       21.0   6 3.90 2.875 17.02  0  1    4    4
#> Datsun 710          22.8   4 3.85 2.320 18.61  1  1    4    1
#> Hornet 4 Drive      21.4   6 3.08 3.215 19.44  1  0    3    1
#> Hornet Sportabout   18.7   8 3.15 3.440 17.02  0  0    3    2
#> Valiant             18.1   6 2.76 3.460 20.22  1  0    3    1
#> Duster 360          14.3   8 3.21 3.570 15.84  0  0    3    4
#> Merc 240D           24.4   4 3.69 3.190 20.00  1  0    4    2
#> Merc 230            22.8   4 3.92 3.150 22.90  1  0    4    2
#> Merc 280            19.2   6 3.92 3.440 18.30  1  0    4    4
#> Merc 280C           17.8   6 3.92 3.440 18.90  1  0    4    4
#> Merc 450SE          16.4   8 3.07 4.070 17.40  0  0    3    3
#> Merc 450SL          17.3   8 3.07 3.730 17.60  0  0    3    3
#> Merc 450SLC         15.2   8 3.07 3.780 18.00  0  0    3    3
#> Cadillac Fleetwood  10.4   8 2.93 5.250 17.98  0  0    3    4
#> Lincoln Continental 10.4   8 3.00 5.424 17.82  0  0    3    4
#> Chrysler Imperial   14.7   8 3.23 5.345 17.42  0  0    3    4
#> Fiat 128            32.4   4 4.08 2.200 19.47  1  1    4    1
#> Honda Civic         30.4   4 4.93 1.615 18.52  1  1    4    2
#> Toyota Corolla      33.9   4 4.22 1.835 19.90  1  1    4    1
#> Toyota Corona       21.5   4 3.70 2.465 20.01  1  0    3    1
#> Dodge Challenger    15.5   8 2.76 3.520 16.87  0  0    3    2
#> AMC Javelin         15.2   8 3.15 3.435 17.30  0  0    3    2
#> Camaro Z28          13.3   8 3.73 3.840 15.41  0  0    3    4
#> Pontiac Firebird    19.2   8 3.08 3.845 17.05  0  0    3    2
#> Fiat X1-9           27.3   4 4.08 1.935 18.90  1  1    4    1
#> Porsche 914-2       26.0   4 4.43 2.140 16.70  0  1    5    2
#> Lotus Europa        30.4   4 3.77 1.513 16.90  1  1    5    2
#> Ford Pantera L      15.8   8 4.22 3.170 14.50  0  1    5    4
#> Ferrari Dino        19.7   6 3.62 2.770 15.50  0  1    5    6
#> Maserati Bora       15.0   8 3.54 3.570 14.60  0  1    5    8
#> Volvo 142E          21.4   4 4.11 2.780 18.60  1  1    4    2

# `&` and `|` take the intersection or the union of two selections:
iris %>% select(starts_with("Petal") & ends_with("Width"))
#>     Petal.Length Petal.Width Sepal.Width
#> 1            1.4         0.2         3.5
#> 2            1.4         0.2         3.0
#> 3            1.3         0.2         3.2
#> 4            1.5         0.2         3.1
#> 5            1.4         0.2         3.6
#> 6            1.7         0.4         3.9
#> 7            1.4         0.3         3.4
#> 8            1.5         0.2         3.4
#> 9            1.4         0.2         2.9
#> 10           1.5         0.1         3.1
#> 11           1.5         0.2         3.7
#> 12           1.6         0.2         3.4
#> 13           1.4         0.1         3.0
#> 14           1.1         0.1         3.0
#> 15           1.2         0.2         4.0
#> 16           1.5         0.4         4.4
#> 17           1.3         0.4         3.9
#> 18           1.4         0.3         3.5
#> 19           1.7         0.3         3.8
#> 20           1.5         0.3         3.8
#> 21           1.7         0.2         3.4
#> 22           1.5         0.4         3.7
#> 23           1.0         0.2         3.6
#> 24           1.7         0.5         3.3
#> 25           1.9         0.2         3.4
#> 26           1.6         0.2         3.0
#> 27           1.6         0.4         3.4
#> 28           1.5         0.2         3.5
#> 29           1.4         0.2         3.4
#> 30           1.6         0.2         3.2
#> 31           1.6         0.2         3.1
#> 32           1.5         0.4         3.4
#> 33           1.5         0.1         4.1
#> 34           1.4         0.2         4.2
#> 35           1.5         0.2         3.1
#> 36           1.2         0.2         3.2
#> 37           1.3         0.2         3.5
#> 38           1.4         0.1         3.6
#> 39           1.3         0.2         3.0
#> 40           1.5         0.2         3.4
#> 41           1.3         0.3         3.5
#> 42           1.3         0.3         2.3
#> 43           1.3         0.2         3.2
#> 44           1.6         0.6         3.5
#> 45           1.9         0.4         3.8
#> 46           1.4         0.3         3.0
#> 47           1.6         0.2         3.8
#> 48           1.4         0.2         3.2
#> 49           1.5         0.2         3.7
#> 50           1.4         0.2         3.3
#> 51           4.7         1.4         3.2
#> 52           4.5         1.5         3.2
#> 53           4.9         1.5         3.1
#> 54           4.0         1.3         2.3
#> 55           4.6         1.5         2.8
#> 56           4.5         1.3         2.8
#> 57           4.7         1.6         3.3
#> 58           3.3         1.0         2.4
#> 59           4.6         1.3         2.9
#> 60           3.9         1.4         2.7
#> 61           3.5         1.0         2.0
#> 62           4.2         1.5         3.0
#> 63           4.0         1.0         2.2
#> 64           4.7         1.4         2.9
#> 65           3.6         1.3         2.9
#> 66           4.4         1.4         3.1
#> 67           4.5         1.5         3.0
#> 68           4.1         1.0         2.7
#> 69           4.5         1.5         2.2
#> 70           3.9         1.1         2.5
#> 71           4.8         1.8         3.2
#> 72           4.0         1.3         2.8
#> 73           4.9         1.5         2.5
#> 74           4.7         1.2         2.8
#> 75           4.3         1.3         2.9
#> 76           4.4         1.4         3.0
#> 77           4.8         1.4         2.8
#> 78           5.0         1.7         3.0
#> 79           4.5         1.5         2.9
#> 80           3.5         1.0         2.6
#> 81           3.8         1.1         2.4
#> 82           3.7         1.0         2.4
#> 83           3.9         1.2         2.7
#> 84           5.1         1.6         2.7
#> 85           4.5         1.5         3.0
#> 86           4.5         1.6         3.4
#> 87           4.7         1.5         3.1
#> 88           4.4         1.3         2.3
#> 89           4.1         1.3         3.0
#> 90           4.0         1.3         2.5
#> 91           4.4         1.2         2.6
#> 92           4.6         1.4         3.0
#> 93           4.0         1.2         2.6
#> 94           3.3         1.0         2.3
#> 95           4.2         1.3         2.7
#> 96           4.2         1.2         3.0
#> 97           4.2         1.3         2.9
#> 98           4.3         1.3         2.9
#> 99           3.0         1.1         2.5
#> 100          4.1         1.3         2.8
#> 101          6.0         2.5         3.3
#> 102          5.1         1.9         2.7
#> 103          5.9         2.1         3.0
#> 104          5.6         1.8         2.9
#> 105          5.8         2.2         3.0
#> 106          6.6         2.1         3.0
#> 107          4.5         1.7         2.5
#> 108          6.3         1.8         2.9
#> 109          5.8         1.8         2.5
#> 110          6.1         2.5         3.6
#> 111          5.1         2.0         3.2
#> 112          5.3         1.9         2.7
#> 113          5.5         2.1         3.0
#> 114          5.0         2.0         2.5
#> 115          5.1         2.4         2.8
#> 116          5.3         2.3         3.2
#> 117          5.5         1.8         3.0
#> 118          6.7         2.2         3.8
#> 119          6.9         2.3         2.6
#> 120          5.0         1.5         2.2
#> 121          5.7         2.3         3.2
#> 122          4.9         2.0         2.8
#> 123          6.7         2.0         2.8
#> 124          4.9         1.8         2.7
#> 125          5.7         2.1         3.3
#> 126          6.0         1.8         3.2
#> 127          4.8         1.8         2.8
#> 128          4.9         1.8         3.0
#> 129          5.6         2.1         2.8
#> 130          5.8         1.6         3.0
#> 131          6.1         1.9         2.8
#> 132          6.4         2.0         3.8
#> 133          5.6         2.2         2.8
#> 134          5.1         1.5         2.8
#> 135          5.6         1.4         2.6
#> 136          6.1         2.3         3.0
#> 137          5.6         2.4         3.4
#> 138          5.5         1.8         3.1
#> 139          4.8         1.8         3.0
#> 140          5.4         2.1         3.1
#> 141          5.6         2.4         3.1
#> 142          5.1         2.3         3.1
#> 143          5.1         1.9         2.7
#> 144          5.9         2.3         3.2
#> 145          5.7         2.5         3.3
#> 146          5.2         2.3         3.0
#> 147          5.0         1.9         2.5
#> 148          5.2         2.0         3.0
#> 149          5.4         2.3         3.4
#> 150          5.1         1.8         3.0
iris %>% select(starts_with("Petal") | ends_with("Width"))
#>     Sepal.Length Sepal.Width Petal.Length Petal.Width    Species
#> 1            5.1         3.5          1.4         0.2     setosa
#> 2            4.9         3.0          1.4         0.2     setosa
#> 3            4.7         3.2          1.3         0.2     setosa
#> 4            4.6         3.1          1.5         0.2     setosa
#> 5            5.0         3.6          1.4         0.2     setosa
#> 6            5.4         3.9          1.7         0.4     setosa
#> 7            4.6         3.4          1.4         0.3     setosa
#> 8            5.0         3.4          1.5         0.2     setosa
#> 9            4.4         2.9          1.4         0.2     setosa
#> 10           4.9         3.1          1.5         0.1     setosa
#> 11           5.4         3.7          1.5         0.2     setosa
#> 12           4.8         3.4          1.6         0.2     setosa
#> 13           4.8         3.0          1.4         0.1     setosa
#> 14           4.3         3.0          1.1         0.1     setosa
#> 15           5.8         4.0          1.2         0.2     setosa
#> 16           5.7         4.4          1.5         0.4     setosa
#> 17           5.4         3.9          1.3         0.4     setosa
#> 18           5.1         3.5          1.4         0.3     setosa
#> 19           5.7         3.8          1.7         0.3     setosa
#> 20           5.1         3.8          1.5         0.3     setosa
#> 21           5.4         3.4          1.7         0.2     setosa
#> 22           5.1         3.7          1.5         0.4     setosa
#> 23           4.6         3.6          1.0         0.2     setosa
#> 24           5.1         3.3          1.7         0.5     setosa
#> 25           4.8         3.4          1.9         0.2     setosa
#> 26           5.0         3.0          1.6         0.2     setosa
#> 27           5.0         3.4          1.6         0.4     setosa
#> 28           5.2         3.5          1.5         0.2     setosa
#> 29           5.2         3.4          1.4         0.2     setosa
#> 30           4.7         3.2          1.6         0.2     setosa
#> 31           4.8         3.1          1.6         0.2     setosa
#> 32           5.4         3.4          1.5         0.4     setosa
#> 33           5.2         4.1          1.5         0.1     setosa
#> 34           5.5         4.2          1.4         0.2     setosa
#> 35           4.9         3.1          1.5         0.2     setosa
#> 36           5.0         3.2          1.2         0.2     setosa
#> 37           5.5         3.5          1.3         0.2     setosa
#> 38           4.9         3.6          1.4         0.1     setosa
#> 39           4.4         3.0          1.3         0.2     setosa
#> 40           5.1         3.4          1.5         0.2     setosa
#> 41           5.0         3.5          1.3         0.3     setosa
#> 42           4.5         2.3          1.3         0.3     setosa
#> 43           4.4         3.2          1.3         0.2     setosa
#> 44           5.0         3.5          1.6         0.6     setosa
#> 45           5.1         3.8          1.9         0.4     setosa
#> 46           4.8         3.0          1.4         0.3     setosa
#> 47           5.1         3.8          1.6         0.2     setosa
#> 48           4.6         3.2          1.4         0.2     setosa
#> 49           5.3         3.7          1.5         0.2     setosa
#> 50           5.0         3.3          1.4         0.2     setosa
#> 51           7.0         3.2          4.7         1.4 versicolor
#> 52           6.4         3.2          4.5         1.5 versicolor
#> 53           6.9         3.1          4.9         1.5 versicolor
#> 54           5.5         2.3          4.0         1.3 versicolor
#> 55           6.5         2.8          4.6         1.5 versicolor
#> 56           5.7         2.8          4.5         1.3 versicolor
#> 57           6.3         3.3          4.7         1.6 versicolor
#> 58           4.9         2.4          3.3         1.0 versicolor
#> 59           6.6         2.9          4.6         1.3 versicolor
#> 60           5.2         2.7          3.9         1.4 versicolor
#> 61           5.0         2.0          3.5         1.0 versicolor
#> 62           5.9         3.0          4.2         1.5 versicolor
#> 63           6.0         2.2          4.0         1.0 versicolor
#> 64           6.1         2.9          4.7         1.4 versicolor
#> 65           5.6         2.9          3.6         1.3 versicolor
#> 66           6.7         3.1          4.4         1.4 versicolor
#> 67           5.6         3.0          4.5         1.5 versicolor
#> 68           5.8         2.7          4.1         1.0 versicolor
#> 69           6.2         2.2          4.5         1.5 versicolor
#> 70           5.6         2.5          3.9         1.1 versicolor
#> 71           5.9         3.2          4.8         1.8 versicolor
#> 72           6.1         2.8          4.0         1.3 versicolor
#> 73           6.3         2.5          4.9         1.5 versicolor
#> 74           6.1         2.8          4.7         1.2 versicolor
#> 75           6.4         2.9          4.3         1.3 versicolor
#> 76           6.6         3.0          4.4         1.4 versicolor
#> 77           6.8         2.8          4.8         1.4 versicolor
#> 78           6.7         3.0          5.0         1.7 versicolor
#> 79           6.0         2.9          4.5         1.5 versicolor
#> 80           5.7         2.6          3.5         1.0 versicolor
#> 81           5.5         2.4          3.8         1.1 versicolor
#> 82           5.5         2.4          3.7         1.0 versicolor
#> 83           5.8         2.7          3.9         1.2 versicolor
#> 84           6.0         2.7          5.1         1.6 versicolor
#> 85           5.4         3.0          4.5         1.5 versicolor
#> 86           6.0         3.4          4.5         1.6 versicolor
#> 87           6.7         3.1          4.7         1.5 versicolor
#> 88           6.3         2.3          4.4         1.3 versicolor
#> 89           5.6         3.0          4.1         1.3 versicolor
#> 90           5.5         2.5          4.0         1.3 versicolor
#> 91           5.5         2.6          4.4         1.2 versicolor
#> 92           6.1         3.0          4.6         1.4 versicolor
#> 93           5.8         2.6          4.0         1.2 versicolor
#> 94           5.0         2.3          3.3         1.0 versicolor
#> 95           5.6         2.7          4.2         1.3 versicolor
#> 96           5.7         3.0          4.2         1.2 versicolor
#> 97           5.7         2.9          4.2         1.3 versicolor
#> 98           6.2         2.9          4.3         1.3 versicolor
#> 99           5.1         2.5          3.0         1.1 versicolor
#> 100          5.7         2.8          4.1         1.3 versicolor
#> 101          6.3         3.3          6.0         2.5  virginica
#> 102          5.8         2.7          5.1         1.9  virginica
#> 103          7.1         3.0          5.9         2.1  virginica
#> 104          6.3         2.9          5.6         1.8  virginica
#> 105          6.5         3.0          5.8         2.2  virginica
#> 106          7.6         3.0          6.6         2.1  virginica
#> 107          4.9         2.5          4.5         1.7  virginica
#> 108          7.3         2.9          6.3         1.8  virginica
#> 109          6.7         2.5          5.8         1.8  virginica
#> 110          7.2         3.6          6.1         2.5  virginica
#> 111          6.5         3.2          5.1         2.0  virginica
#> 112          6.4         2.7          5.3         1.9  virginica
#> 113          6.8         3.0          5.5         2.1  virginica
#> 114          5.7         2.5          5.0         2.0  virginica
#> 115          5.8         2.8          5.1         2.4  virginica
#> 116          6.4         3.2          5.3         2.3  virginica
#> 117          6.5         3.0          5.5         1.8  virginica
#> 118          7.7         3.8          6.7         2.2  virginica
#> 119          7.7         2.6          6.9         2.3  virginica
#> 120          6.0         2.2          5.0         1.5  virginica
#> 121          6.9         3.2          5.7         2.3  virginica
#> 122          5.6         2.8          4.9         2.0  virginica
#> 123          7.7         2.8          6.7         2.0  virginica
#> 124          6.3         2.7          4.9         1.8  virginica
#> 125          6.7         3.3          5.7         2.1  virginica
#> 126          7.2         3.2          6.0         1.8  virginica
#> 127          6.2         2.8          4.8         1.8  virginica
#> 128          6.1         3.0          4.9         1.8  virginica
#> 129          6.4         2.8          5.6         2.1  virginica
#> 130          7.2         3.0          5.8         1.6  virginica
#> 131          7.4         2.8          6.1         1.9  virginica
#> 132          7.9         3.8          6.4         2.0  virginica
#> 133          6.4         2.8          5.6         2.2  virginica
#> 134          6.3         2.8          5.1         1.5  virginica
#> 135          6.1         2.6          5.6         1.4  virginica
#> 136          7.7         3.0          6.1         2.3  virginica
#> 137          6.3         3.4          5.6         2.4  virginica
#> 138          6.4         3.1          5.5         1.8  virginica
#> 139          6.0         3.0          4.8         1.8  virginica
#> 140          6.9         3.1          5.4         2.1  virginica
#> 141          6.7         3.1          5.6         2.4  virginica
#> 142          6.9         3.1          5.1         2.3  virginica
#> 143          5.8         2.7          5.1         1.9  virginica
#> 144          6.8         3.2          5.9         2.3  virginica
#> 145          6.7         3.3          5.7         2.5  virginica
#> 146          6.7         3.0          5.2         2.3  virginica
#> 147          6.3         2.5          5.0         1.9  virginica
#> 148          6.5         3.0          5.2         2.0  virginica
#> 149          6.2         3.4          5.4         2.3  virginica
#> 150          5.9         3.0          5.1         1.8  virginica

# To take the difference between two selections, combine the `&` and
# `!` operators:
iris %>% select(starts_with("Petal") & !ends_with("Width"))
#>     Petal.Length
#> 1            1.4
#> 2            1.4
#> 3            1.3
#> 4            1.5
#> 5            1.4
#> 6            1.7
#> 7            1.4
#> 8            1.5
#> 9            1.4
#> 10           1.5
#> 11           1.5
#> 12           1.6
#> 13           1.4
#> 14           1.1
#> 15           1.2
#> 16           1.5
#> 17           1.3
#> 18           1.4
#> 19           1.7
#> 20           1.5
#> 21           1.7
#> 22           1.5
#> 23           1.0
#> 24           1.7
#> 25           1.9
#> 26           1.6
#> 27           1.6
#> 28           1.5
#> 29           1.4
#> 30           1.6
#> 31           1.6
#> 32           1.5
#> 33           1.5
#> 34           1.4
#> 35           1.5
#> 36           1.2
#> 37           1.3
#> 38           1.4
#> 39           1.3
#> 40           1.5
#> 41           1.3
#> 42           1.3
#> 43           1.3
#> 44           1.6
#> 45           1.9
#> 46           1.4
#> 47           1.6
#> 48           1.4
#> 49           1.5
#> 50           1.4
#> 51           4.7
#> 52           4.5
#> 53           4.9
#> 54           4.0
#> 55           4.6
#> 56           4.5
#> 57           4.7
#> 58           3.3
#> 59           4.6
#> 60           3.9
#> 61           3.5
#> 62           4.2
#> 63           4.0
#> 64           4.7
#> 65           3.6
#> 66           4.4
#> 67           4.5
#> 68           4.1
#> 69           4.5
#> 70           3.9
#> 71           4.8
#> 72           4.0
#> 73           4.9
#> 74           4.7
#> 75           4.3
#> 76           4.4
#> 77           4.8
#> 78           5.0
#> 79           4.5
#> 80           3.5
#> 81           3.8
#> 82           3.7
#> 83           3.9
#> 84           5.1
#> 85           4.5
#> 86           4.5
#> 87           4.7
#> 88           4.4
#> 89           4.1
#> 90           4.0
#> 91           4.4
#> 92           4.6
#> 93           4.0
#> 94           3.3
#> 95           4.2
#> 96           4.2
#> 97           4.2
#> 98           4.3
#> 99           3.0
#> 100          4.1
#> 101          6.0
#> 102          5.1
#> 103          5.9
#> 104          5.6
#> 105          5.8
#> 106          6.6
#> 107          4.5
#> 108          6.3
#> 109          5.8
#> 110          6.1
#> 111          5.1
#> 112          5.3
#> 113          5.5
#> 114          5.0
#> 115          5.1
#> 116          5.3
#> 117          5.5
#> 118          6.7
#> 119          6.9
#> 120          5.0
#> 121          5.7
#> 122          4.9
#> 123          6.7
#> 124          4.9
#> 125          5.7
#> 126          6.0
#> 127          4.8
#> 128          4.9
#> 129          5.6
#> 130          5.8
#> 131          6.1
#> 132          6.4
#> 133          5.6
#> 134          5.1
#> 135          5.6
#> 136          6.1
#> 137          5.6
#> 138          5.5
#> 139          4.8
#> 140          5.4
#> 141          5.6
#> 142          5.1
#> 143          5.1
#> 144          5.9
#> 145          5.7
#> 146          5.2
#> 147          5.0
#> 148          5.2
#> 149          5.4
#> 150          5.1
```
