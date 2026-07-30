# Select Helpers

These functions allow you to select variables based on their names.

- `starts_with()`: Starts with a prefix.

- `ends_with()`: Ends with a prefix.

- `contains()`: Contains a literal string.

- `matches()`: Matches a regular expression.

- `all_of()`: Matches variable names in a character vector. All names
  must be present, otherwise an error is thrown.

- `any_of()`: The same as `all_of()` except it doesn't throw an error.

- `everything()`: Matches all variables.

- `last_col()`: Select the last variable, possibly with an offset.

## Usage

``` r
starts_with(match, ignore.case = TRUE, vars = peek_vars())

ends_with(match, ignore.case = TRUE, vars = peek_vars())

contains(match, ignore.case = TRUE, vars = peek_vars())

matches(match, ignore.case = TRUE, perl = FALSE, vars = peek_vars())

num_range(prefix, range, width = NULL, vars = peek_vars())

all_of(x, vars = peek_vars())

any_of(x, vars = peek_vars())

everything(vars = peek_vars())

last_col(offset = 0L, vars = peek_vars())
```

## Arguments

- match:

  `character(n)`. If length \> 1, the union of the matches is taken.

- ignore.case:

  `logical(1)`. If `TRUE`, the default, ignores case when matching
  names.

- vars:

  `character(n)`. A character vector of variable names. When called from
  inside selecting functions such as [`select()`](select.md), these are
  automatically set to the names of the table.

- perl:

  `logical(1)`. Should Perl-compatible regexps be used?

- prefix:

  A prefix which starts the numeric range.

- range:

  `integer(n)`. A sequence of integers, e.g. `1:5`.

- width:

  `numeric(1)`. Optionally, the "width" of the numeric range. For
  example, a range of 2 gives "01", a range of three "001", etc.

- x:

  `character(n)`. A vector of column names.

- offset:

  `integer(1)`. Select the `n`th variable from the end of the
  `data.frame`.

## Value

An integer vector giving the position of the matched variables.

## See also

[`select()`](select.md), [`relocate()`](relocate.md),
[`where()`](where.md), [`group_cols()`](group_cols.md)

## Examples

``` r
mtcars %>% select(starts_with("c"))
#>                     cyl carb
#> Mazda RX4             6    4
#> Mazda RX4 Wag         6    4
#> Datsun 710            4    1
#> Hornet 4 Drive        6    1
#> Hornet Sportabout     8    2
#> Valiant               6    1
#> Duster 360            8    4
#> Merc 240D             4    2
#> Merc 230              4    2
#> Merc 280              6    4
#> Merc 280C             6    4
#> Merc 450SE            8    3
#> Merc 450SL            8    3
#> Merc 450SLC           8    3
#> Cadillac Fleetwood    8    4
#> Lincoln Continental   8    4
#> Chrysler Imperial     8    4
#> Fiat 128              4    1
#> Honda Civic           4    2
#> Toyota Corolla        4    1
#> Toyota Corona         4    1
#> Dodge Challenger      8    2
#> AMC Javelin           8    2
#> Camaro Z28            8    4
#> Pontiac Firebird      8    2
#> Fiat X1-9             4    1
#> Porsche 914-2         4    2
#> Lotus Europa          4    2
#> Ford Pantera L        8    4
#> Ferrari Dino          6    6
#> Maserati Bora         8    8
#> Volvo 142E            4    2
mtcars %>% select(starts_with(c("c", "h")))
#>                     cyl  hp carb
#> Mazda RX4             6 110    4
#> Mazda RX4 Wag         6 110    4
#> Datsun 710            4  93    1
#> Hornet 4 Drive        6 110    1
#> Hornet Sportabout     8 175    2
#> Valiant               6 105    1
#> Duster 360            8 245    4
#> Merc 240D             4  62    2
#> Merc 230              4  95    2
#> Merc 280              6 123    4
#> Merc 280C             6 123    4
#> Merc 450SE            8 180    3
#> Merc 450SL            8 180    3
#> Merc 450SLC           8 180    3
#> Cadillac Fleetwood    8 205    4
#> Lincoln Continental   8 215    4
#> Chrysler Imperial     8 230    4
#> Fiat 128              4  66    1
#> Honda Civic           4  52    2
#> Toyota Corolla        4  65    1
#> Toyota Corona         4  97    1
#> Dodge Challenger      8 150    2
#> AMC Javelin           8 150    2
#> Camaro Z28            8 245    4
#> Pontiac Firebird      8 175    2
#> Fiat X1-9             4  66    1
#> Porsche 914-2         4  91    2
#> Lotus Europa          4 113    2
#> Ford Pantera L        8 264    4
#> Ferrari Dino          6 175    6
#> Maserati Bora         8 335    8
#> Volvo 142E            4 109    2
mtcars %>% select(ends_with("b"))
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
mtcars %>% relocate(contains("a"), .before = mpg)
#>                     drat am gear carb  mpg cyl  disp  hp    wt  qsec vs
#> Mazda RX4           3.90  1    4    4 21.0   6 160.0 110 2.620 16.46  0
#> Mazda RX4 Wag       3.90  1    4    4 21.0   6 160.0 110 2.875 17.02  0
#> Datsun 710          3.85  1    4    1 22.8   4 108.0  93 2.320 18.61  1
#> Hornet 4 Drive      3.08  0    3    1 21.4   6 258.0 110 3.215 19.44  1
#> Hornet Sportabout   3.15  0    3    2 18.7   8 360.0 175 3.440 17.02  0
#> Valiant             2.76  0    3    1 18.1   6 225.0 105 3.460 20.22  1
#> Duster 360          3.21  0    3    4 14.3   8 360.0 245 3.570 15.84  0
#> Merc 240D           3.69  0    4    2 24.4   4 146.7  62 3.190 20.00  1
#> Merc 230            3.92  0    4    2 22.8   4 140.8  95 3.150 22.90  1
#> Merc 280            3.92  0    4    4 19.2   6 167.6 123 3.440 18.30  1
#> Merc 280C           3.92  0    4    4 17.8   6 167.6 123 3.440 18.90  1
#> Merc 450SE          3.07  0    3    3 16.4   8 275.8 180 4.070 17.40  0
#> Merc 450SL          3.07  0    3    3 17.3   8 275.8 180 3.730 17.60  0
#> Merc 450SLC         3.07  0    3    3 15.2   8 275.8 180 3.780 18.00  0
#> Cadillac Fleetwood  2.93  0    3    4 10.4   8 472.0 205 5.250 17.98  0
#> Lincoln Continental 3.00  0    3    4 10.4   8 460.0 215 5.424 17.82  0
#> Chrysler Imperial   3.23  0    3    4 14.7   8 440.0 230 5.345 17.42  0
#> Fiat 128            4.08  1    4    1 32.4   4  78.7  66 2.200 19.47  1
#> Honda Civic         4.93  1    4    2 30.4   4  75.7  52 1.615 18.52  1
#> Toyota Corolla      4.22  1    4    1 33.9   4  71.1  65 1.835 19.90  1
#> Toyota Corona       3.70  0    3    1 21.5   4 120.1  97 2.465 20.01  1
#> Dodge Challenger    2.76  0    3    2 15.5   8 318.0 150 3.520 16.87  0
#> AMC Javelin         3.15  0    3    2 15.2   8 304.0 150 3.435 17.30  0
#> Camaro Z28          3.73  0    3    4 13.3   8 350.0 245 3.840 15.41  0
#> Pontiac Firebird    3.08  0    3    2 19.2   8 400.0 175 3.845 17.05  0
#> Fiat X1-9           4.08  1    4    1 27.3   4  79.0  66 1.935 18.90  1
#> Porsche 914-2       4.43  1    5    2 26.0   4 120.3  91 2.140 16.70  0
#> Lotus Europa        3.77  1    5    2 30.4   4  95.1 113 1.513 16.90  1
#> Ford Pantera L      4.22  1    5    4 15.8   8 351.0 264 3.170 14.50  0
#> Ferrari Dino        3.62  1    5    6 19.7   6 145.0 175 2.770 15.50  0
#> Maserati Bora       3.54  1    5    8 15.0   8 301.0 335 3.570 14.60  0
#> Volvo 142E          4.11  1    4    2 21.4   4 121.0 109 2.780 18.60  1
iris %>% select(matches(".t."))
#>     Sepal.Length Sepal.Width Petal.Length Petal.Width
#> 1            5.1         3.5          1.4         0.2
#> 2            4.9         3.0          1.4         0.2
#> 3            4.7         3.2          1.3         0.2
#> 4            4.6         3.1          1.5         0.2
#> 5            5.0         3.6          1.4         0.2
#> 6            5.4         3.9          1.7         0.4
#> 7            4.6         3.4          1.4         0.3
#> 8            5.0         3.4          1.5         0.2
#> 9            4.4         2.9          1.4         0.2
#> 10           4.9         3.1          1.5         0.1
#> 11           5.4         3.7          1.5         0.2
#> 12           4.8         3.4          1.6         0.2
#> 13           4.8         3.0          1.4         0.1
#> 14           4.3         3.0          1.1         0.1
#> 15           5.8         4.0          1.2         0.2
#> 16           5.7         4.4          1.5         0.4
#> 17           5.4         3.9          1.3         0.4
#> 18           5.1         3.5          1.4         0.3
#> 19           5.7         3.8          1.7         0.3
#> 20           5.1         3.8          1.5         0.3
#> 21           5.4         3.4          1.7         0.2
#> 22           5.1         3.7          1.5         0.4
#> 23           4.6         3.6          1.0         0.2
#> 24           5.1         3.3          1.7         0.5
#> 25           4.8         3.4          1.9         0.2
#> 26           5.0         3.0          1.6         0.2
#> 27           5.0         3.4          1.6         0.4
#> 28           5.2         3.5          1.5         0.2
#> 29           5.2         3.4          1.4         0.2
#> 30           4.7         3.2          1.6         0.2
#> 31           4.8         3.1          1.6         0.2
#> 32           5.4         3.4          1.5         0.4
#> 33           5.2         4.1          1.5         0.1
#> 34           5.5         4.2          1.4         0.2
#> 35           4.9         3.1          1.5         0.2
#> 36           5.0         3.2          1.2         0.2
#> 37           5.5         3.5          1.3         0.2
#> 38           4.9         3.6          1.4         0.1
#> 39           4.4         3.0          1.3         0.2
#> 40           5.1         3.4          1.5         0.2
#> 41           5.0         3.5          1.3         0.3
#> 42           4.5         2.3          1.3         0.3
#> 43           4.4         3.2          1.3         0.2
#> 44           5.0         3.5          1.6         0.6
#> 45           5.1         3.8          1.9         0.4
#> 46           4.8         3.0          1.4         0.3
#> 47           5.1         3.8          1.6         0.2
#> 48           4.6         3.2          1.4         0.2
#> 49           5.3         3.7          1.5         0.2
#> 50           5.0         3.3          1.4         0.2
#> 51           7.0         3.2          4.7         1.4
#> 52           6.4         3.2          4.5         1.5
#> 53           6.9         3.1          4.9         1.5
#> 54           5.5         2.3          4.0         1.3
#> 55           6.5         2.8          4.6         1.5
#> 56           5.7         2.8          4.5         1.3
#> 57           6.3         3.3          4.7         1.6
#> 58           4.9         2.4          3.3         1.0
#> 59           6.6         2.9          4.6         1.3
#> 60           5.2         2.7          3.9         1.4
#> 61           5.0         2.0          3.5         1.0
#> 62           5.9         3.0          4.2         1.5
#> 63           6.0         2.2          4.0         1.0
#> 64           6.1         2.9          4.7         1.4
#> 65           5.6         2.9          3.6         1.3
#> 66           6.7         3.1          4.4         1.4
#> 67           5.6         3.0          4.5         1.5
#> 68           5.8         2.7          4.1         1.0
#> 69           6.2         2.2          4.5         1.5
#> 70           5.6         2.5          3.9         1.1
#> 71           5.9         3.2          4.8         1.8
#> 72           6.1         2.8          4.0         1.3
#> 73           6.3         2.5          4.9         1.5
#> 74           6.1         2.8          4.7         1.2
#> 75           6.4         2.9          4.3         1.3
#> 76           6.6         3.0          4.4         1.4
#> 77           6.8         2.8          4.8         1.4
#> 78           6.7         3.0          5.0         1.7
#> 79           6.0         2.9          4.5         1.5
#> 80           5.7         2.6          3.5         1.0
#> 81           5.5         2.4          3.8         1.1
#> 82           5.5         2.4          3.7         1.0
#> 83           5.8         2.7          3.9         1.2
#> 84           6.0         2.7          5.1         1.6
#> 85           5.4         3.0          4.5         1.5
#> 86           6.0         3.4          4.5         1.6
#> 87           6.7         3.1          4.7         1.5
#> 88           6.3         2.3          4.4         1.3
#> 89           5.6         3.0          4.1         1.3
#> 90           5.5         2.5          4.0         1.3
#> 91           5.5         2.6          4.4         1.2
#> 92           6.1         3.0          4.6         1.4
#> 93           5.8         2.6          4.0         1.2
#> 94           5.0         2.3          3.3         1.0
#> 95           5.6         2.7          4.2         1.3
#> 96           5.7         3.0          4.2         1.2
#> 97           5.7         2.9          4.2         1.3
#> 98           6.2         2.9          4.3         1.3
#> 99           5.1         2.5          3.0         1.1
#> 100          5.7         2.8          4.1         1.3
#> 101          6.3         3.3          6.0         2.5
#> 102          5.8         2.7          5.1         1.9
#> 103          7.1         3.0          5.9         2.1
#> 104          6.3         2.9          5.6         1.8
#> 105          6.5         3.0          5.8         2.2
#> 106          7.6         3.0          6.6         2.1
#> 107          4.9         2.5          4.5         1.7
#> 108          7.3         2.9          6.3         1.8
#> 109          6.7         2.5          5.8         1.8
#> 110          7.2         3.6          6.1         2.5
#> 111          6.5         3.2          5.1         2.0
#> 112          6.4         2.7          5.3         1.9
#> 113          6.8         3.0          5.5         2.1
#> 114          5.7         2.5          5.0         2.0
#> 115          5.8         2.8          5.1         2.4
#> 116          6.4         3.2          5.3         2.3
#> 117          6.5         3.0          5.5         1.8
#> 118          7.7         3.8          6.7         2.2
#> 119          7.7         2.6          6.9         2.3
#> 120          6.0         2.2          5.0         1.5
#> 121          6.9         3.2          5.7         2.3
#> 122          5.6         2.8          4.9         2.0
#> 123          7.7         2.8          6.7         2.0
#> 124          6.3         2.7          4.9         1.8
#> 125          6.7         3.3          5.7         2.1
#> 126          7.2         3.2          6.0         1.8
#> 127          6.2         2.8          4.8         1.8
#> 128          6.1         3.0          4.9         1.8
#> 129          6.4         2.8          5.6         2.1
#> 130          7.2         3.0          5.8         1.6
#> 131          7.4         2.8          6.1         1.9
#> 132          7.9         3.8          6.4         2.0
#> 133          6.4         2.8          5.6         2.2
#> 134          6.3         2.8          5.1         1.5
#> 135          6.1         2.6          5.6         1.4
#> 136          7.7         3.0          6.1         2.3
#> 137          6.3         3.4          5.6         2.4
#> 138          6.4         3.1          5.5         1.8
#> 139          6.0         3.0          4.8         1.8
#> 140          6.9         3.1          5.4         2.1
#> 141          6.7         3.1          5.6         2.4
#> 142          6.9         3.1          5.1         2.3
#> 143          5.8         2.7          5.1         1.9
#> 144          6.8         3.2          5.9         2.3
#> 145          6.7         3.3          5.7         2.5
#> 146          6.7         3.0          5.2         2.3
#> 147          6.3         2.5          5.0         1.9
#> 148          6.5         3.0          5.2         2.0
#> 149          6.2         3.4          5.4         2.3
#> 150          5.9         3.0          5.1         1.8
mtcars %>% select(last_col())
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

# `all_of()` selects the variables in a character vector:
iris %>% select(all_of(c("Petal.Length", "Petal.Width")))
#>     Petal.Length Petal.Width
#> 1            1.4         0.2
#> 2            1.4         0.2
#> 3            1.3         0.2
#> 4            1.5         0.2
#> 5            1.4         0.2
#> 6            1.7         0.4
#> 7            1.4         0.3
#> 8            1.5         0.2
#> 9            1.4         0.2
#> 10           1.5         0.1
#> 11           1.5         0.2
#> 12           1.6         0.2
#> 13           1.4         0.1
#> 14           1.1         0.1
#> 15           1.2         0.2
#> 16           1.5         0.4
#> 17           1.3         0.4
#> 18           1.4         0.3
#> 19           1.7         0.3
#> 20           1.5         0.3
#> 21           1.7         0.2
#> 22           1.5         0.4
#> 23           1.0         0.2
#> 24           1.7         0.5
#> 25           1.9         0.2
#> 26           1.6         0.2
#> 27           1.6         0.4
#> 28           1.5         0.2
#> 29           1.4         0.2
#> 30           1.6         0.2
#> 31           1.6         0.2
#> 32           1.5         0.4
#> 33           1.5         0.1
#> 34           1.4         0.2
#> 35           1.5         0.2
#> 36           1.2         0.2
#> 37           1.3         0.2
#> 38           1.4         0.1
#> 39           1.3         0.2
#> 40           1.5         0.2
#> 41           1.3         0.3
#> 42           1.3         0.3
#> 43           1.3         0.2
#> 44           1.6         0.6
#> 45           1.9         0.4
#> 46           1.4         0.3
#> 47           1.6         0.2
#> 48           1.4         0.2
#> 49           1.5         0.2
#> 50           1.4         0.2
#> 51           4.7         1.4
#> 52           4.5         1.5
#> 53           4.9         1.5
#> 54           4.0         1.3
#> 55           4.6         1.5
#> 56           4.5         1.3
#> 57           4.7         1.6
#> 58           3.3         1.0
#> 59           4.6         1.3
#> 60           3.9         1.4
#> 61           3.5         1.0
#> 62           4.2         1.5
#> 63           4.0         1.0
#> 64           4.7         1.4
#> 65           3.6         1.3
#> 66           4.4         1.4
#> 67           4.5         1.5
#> 68           4.1         1.0
#> 69           4.5         1.5
#> 70           3.9         1.1
#> 71           4.8         1.8
#> 72           4.0         1.3
#> 73           4.9         1.5
#> 74           4.7         1.2
#> 75           4.3         1.3
#> 76           4.4         1.4
#> 77           4.8         1.4
#> 78           5.0         1.7
#> 79           4.5         1.5
#> 80           3.5         1.0
#> 81           3.8         1.1
#> 82           3.7         1.0
#> 83           3.9         1.2
#> 84           5.1         1.6
#> 85           4.5         1.5
#> 86           4.5         1.6
#> 87           4.7         1.5
#> 88           4.4         1.3
#> 89           4.1         1.3
#> 90           4.0         1.3
#> 91           4.4         1.2
#> 92           4.6         1.4
#> 93           4.0         1.2
#> 94           3.3         1.0
#> 95           4.2         1.3
#> 96           4.2         1.2
#> 97           4.2         1.3
#> 98           4.3         1.3
#> 99           3.0         1.1
#> 100          4.1         1.3
#> 101          6.0         2.5
#> 102          5.1         1.9
#> 103          5.9         2.1
#> 104          5.6         1.8
#> 105          5.8         2.2
#> 106          6.6         2.1
#> 107          4.5         1.7
#> 108          6.3         1.8
#> 109          5.8         1.8
#> 110          6.1         2.5
#> 111          5.1         2.0
#> 112          5.3         1.9
#> 113          5.5         2.1
#> 114          5.0         2.0
#> 115          5.1         2.4
#> 116          5.3         2.3
#> 117          5.5         1.8
#> 118          6.7         2.2
#> 119          6.9         2.3
#> 120          5.0         1.5
#> 121          5.7         2.3
#> 122          4.9         2.0
#> 123          6.7         2.0
#> 124          4.9         1.8
#> 125          5.7         2.1
#> 126          6.0         1.8
#> 127          4.8         1.8
#> 128          4.9         1.8
#> 129          5.6         2.1
#> 130          5.8         1.6
#> 131          6.1         1.9
#> 132          6.4         2.0
#> 133          5.6         2.2
#> 134          5.1         1.5
#> 135          5.6         1.4
#> 136          6.1         2.3
#> 137          5.6         2.4
#> 138          5.5         1.8
#> 139          4.8         1.8
#> 140          5.4         2.1
#> 141          5.6         2.4
#> 142          5.1         2.3
#> 143          5.1         1.9
#> 144          5.9         2.3
#> 145          5.7         2.5
#> 146          5.2         2.3
#> 147          5.0         1.9
#> 148          5.2         2.0
#> 149          5.4         2.3
#> 150          5.1         1.8
# `all_of()` is strict and will throw an error if the column name isn't found
try({iris %>% select(all_of(c("Species", "Genres")))})
#> Error in all_of(c("Species", "Genres")) : 
#>   The column Genres does not exist.
# However `any_of()` allows missing variables
iris %>% select(any_of(c("Species", "Genres")))
#>        Species
#> 1       setosa
#> 2       setosa
#> 3       setosa
#> 4       setosa
#> 5       setosa
#> 6       setosa
#> 7       setosa
#> 8       setosa
#> 9       setosa
#> 10      setosa
#> 11      setosa
#> 12      setosa
#> 13      setosa
#> 14      setosa
#> 15      setosa
#> 16      setosa
#> 17      setosa
#> 18      setosa
#> 19      setosa
#> 20      setosa
#> 21      setosa
#> 22      setosa
#> 23      setosa
#> 24      setosa
#> 25      setosa
#> 26      setosa
#> 27      setosa
#> 28      setosa
#> 29      setosa
#> 30      setosa
#> 31      setosa
#> 32      setosa
#> 33      setosa
#> 34      setosa
#> 35      setosa
#> 36      setosa
#> 37      setosa
#> 38      setosa
#> 39      setosa
#> 40      setosa
#> 41      setosa
#> 42      setosa
#> 43      setosa
#> 44      setosa
#> 45      setosa
#> 46      setosa
#> 47      setosa
#> 48      setosa
#> 49      setosa
#> 50      setosa
#> 51  versicolor
#> 52  versicolor
#> 53  versicolor
#> 54  versicolor
#> 55  versicolor
#> 56  versicolor
#> 57  versicolor
#> 58  versicolor
#> 59  versicolor
#> 60  versicolor
#> 61  versicolor
#> 62  versicolor
#> 63  versicolor
#> 64  versicolor
#> 65  versicolor
#> 66  versicolor
#> 67  versicolor
#> 68  versicolor
#> 69  versicolor
#> 70  versicolor
#> 71  versicolor
#> 72  versicolor
#> 73  versicolor
#> 74  versicolor
#> 75  versicolor
#> 76  versicolor
#> 77  versicolor
#> 78  versicolor
#> 79  versicolor
#> 80  versicolor
#> 81  versicolor
#> 82  versicolor
#> 83  versicolor
#> 84  versicolor
#> 85  versicolor
#> 86  versicolor
#> 87  versicolor
#> 88  versicolor
#> 89  versicolor
#> 90  versicolor
#> 91  versicolor
#> 92  versicolor
#> 93  versicolor
#> 94  versicolor
#> 95  versicolor
#> 96  versicolor
#> 97  versicolor
#> 98  versicolor
#> 99  versicolor
#> 100 versicolor
#> 101  virginica
#> 102  virginica
#> 103  virginica
#> 104  virginica
#> 105  virginica
#> 106  virginica
#> 107  virginica
#> 108  virginica
#> 109  virginica
#> 110  virginica
#> 111  virginica
#> 112  virginica
#> 113  virginica
#> 114  virginica
#> 115  virginica
#> 116  virginica
#> 117  virginica
#> 118  virginica
#> 119  virginica
#> 120  virginica
#> 121  virginica
#> 122  virginica
#> 123  virginica
#> 124  virginica
#> 125  virginica
#> 126  virginica
#> 127  virginica
#> 128  virginica
#> 129  virginica
#> 130  virginica
#> 131  virginica
#> 132  virginica
#> 133  virginica
#> 134  virginica
#> 135  virginica
#> 136  virginica
#> 137  virginica
#> 138  virginica
#> 139  virginica
#> 140  virginica
#> 141  virginica
#> 142  virginica
#> 143  virginica
#> 144  virginica
#> 145  virginica
#> 146  virginica
#> 147  virginica
#> 148  virginica
#> 149  virginica
#> 150  virginica
```
