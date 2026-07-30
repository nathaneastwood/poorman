# Split data.frame by groups

`group_split()` works like
[`base::split()`](https://rdrr.io/r/base/split.html) but

- it uses the grouping structure from [`group_by()`](group_by.md) and is
  therefore subject to the data mask

- it does not name the elements of the list based on the grouping as
  this typically loses information and is confusing

## Usage

``` r
group_split(.data, ..., .keep = TRUE)

group_keys(.data)
```

## Arguments

- .data:

  A `data.frame`.

- ...:

  Grouping specification, forwarded to [`group_by()`](group_by.md).

- .keep:

  `logical(1)`. Should the grouping columns be kept (default: TRUE)?

## Value

- `group_split()` returns a list of `data.frame`s. Each `data.frame`
  contains the rows of `.data` with the associated group and all the
  columns, including the grouping variables.

- `group_keys()` returns a `data.frame` with one row per group, and one
  column per grouping variable

## Details

**Grouped `data.frame`s:**

The primary use case for `group_split()` is with already grouped
`data.frame`s, typically a result of [`group_by()`](group_by.md). In
this case, `group_split()` only uses the first argument, the grouped
`data.frame`, and warns when `...` is used.

Because some of these groups may be empty, it is best paired with
`group_keys()` which identifies the representatives of each grouping
variable for the group.

**Ungrouped `data.frame`s:**

When used on ungrouped `data.frame`s, `group_split()` forwards the `...`
to [`group_by()`](group_by.md) before the split, therefore the `...` are
subject to the data mask.

## See also

[`group_by()`](group_by.md)

## Examples

``` r
# Grouped data.frames:
mtcars %>% group_by(cyl, am) %>% group_split()
#> [[1]]
#>                mpg cyl  disp hp drat    wt  qsec vs am gear carb
#> Merc 240D     24.4   4 146.7 62 3.69 3.190 20.00  1  0    4    2
#> Merc 230      22.8   4 140.8 95 3.92 3.150 22.90  1  0    4    2
#> Toyota Corona 21.5   4 120.1 97 3.70 2.465 20.01  1  0    3    1
#> 
#> [[2]]
#>                 mpg cyl  disp  hp drat    wt  qsec vs am gear carb
#> Hornet 4 Drive 21.4   6 258.0 110 3.08 3.215 19.44  1  0    3    1
#> Valiant        18.1   6 225.0 105 2.76 3.460 20.22  1  0    3    1
#> Merc 280       19.2   6 167.6 123 3.92 3.440 18.30  1  0    4    4
#> Merc 280C      17.8   6 167.6 123 3.92 3.440 18.90  1  0    4    4
#> 
#> [[3]]
#>                      mpg cyl  disp  hp drat    wt  qsec vs am gear carb
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
#> 
#> [[4]]
#>                 mpg cyl  disp  hp drat    wt  qsec vs am gear carb
#> Datsun 710     22.8   4 108.0  93 3.85 2.320 18.61  1  1    4    1
#> Fiat 128       32.4   4  78.7  66 4.08 2.200 19.47  1  1    4    1
#> Honda Civic    30.4   4  75.7  52 4.93 1.615 18.52  1  1    4    2
#> Toyota Corolla 33.9   4  71.1  65 4.22 1.835 19.90  1  1    4    1
#> Fiat X1-9      27.3   4  79.0  66 4.08 1.935 18.90  1  1    4    1
#> Porsche 914-2  26.0   4 120.3  91 4.43 2.140 16.70  0  1    5    2
#> Lotus Europa   30.4   4  95.1 113 3.77 1.513 16.90  1  1    5    2
#> Volvo 142E     21.4   4 121.0 109 4.11 2.780 18.60  1  1    4    2
#> 
#> [[5]]
#>                mpg cyl disp  hp drat    wt  qsec vs am gear carb
#> Mazda RX4     21.0   6  160 110 3.90 2.620 16.46  0  1    4    4
#> Mazda RX4 Wag 21.0   6  160 110 3.90 2.875 17.02  0  1    4    4
#> Ferrari Dino  19.7   6  145 175 3.62 2.770 15.50  0  1    5    6
#> 
#> [[6]]
#>                 mpg cyl disp  hp drat   wt qsec vs am gear carb
#> Ford Pantera L 15.8   8  351 264 4.22 3.17 14.5  0  1    5    4
#> Maserati Bora  15.0   8  301 335 3.54 3.57 14.6  0  1    5    8
#> 
mtcars %>% group_by(cyl, am) %>% group_split(.keep = FALSE)
#> [[1]]
#>                mpg  disp hp drat    wt  qsec vs gear carb
#> Merc 240D     24.4 146.7 62 3.69 3.190 20.00  1    4    2
#> Merc 230      22.8 140.8 95 3.92 3.150 22.90  1    4    2
#> Toyota Corona 21.5 120.1 97 3.70 2.465 20.01  1    3    1
#> 
#> [[2]]
#>                 mpg  disp  hp drat    wt  qsec vs gear carb
#> Hornet 4 Drive 21.4 258.0 110 3.08 3.215 19.44  1    3    1
#> Valiant        18.1 225.0 105 2.76 3.460 20.22  1    3    1
#> Merc 280       19.2 167.6 123 3.92 3.440 18.30  1    4    4
#> Merc 280C      17.8 167.6 123 3.92 3.440 18.90  1    4    4
#> 
#> [[3]]
#>                      mpg  disp  hp drat    wt  qsec vs gear carb
#> Hornet Sportabout   18.7 360.0 175 3.15 3.440 17.02  0    3    2
#> Duster 360          14.3 360.0 245 3.21 3.570 15.84  0    3    4
#> Merc 450SE          16.4 275.8 180 3.07 4.070 17.40  0    3    3
#> Merc 450SL          17.3 275.8 180 3.07 3.730 17.60  0    3    3
#> Merc 450SLC         15.2 275.8 180 3.07 3.780 18.00  0    3    3
#> Cadillac Fleetwood  10.4 472.0 205 2.93 5.250 17.98  0    3    4
#> Lincoln Continental 10.4 460.0 215 3.00 5.424 17.82  0    3    4
#> Chrysler Imperial   14.7 440.0 230 3.23 5.345 17.42  0    3    4
#> Dodge Challenger    15.5 318.0 150 2.76 3.520 16.87  0    3    2
#> AMC Javelin         15.2 304.0 150 3.15 3.435 17.30  0    3    2
#> Camaro Z28          13.3 350.0 245 3.73 3.840 15.41  0    3    4
#> Pontiac Firebird    19.2 400.0 175 3.08 3.845 17.05  0    3    2
#> 
#> [[4]]
#>                 mpg  disp  hp drat    wt  qsec vs gear carb
#> Datsun 710     22.8 108.0  93 3.85 2.320 18.61  1    4    1
#> Fiat 128       32.4  78.7  66 4.08 2.200 19.47  1    4    1
#> Honda Civic    30.4  75.7  52 4.93 1.615 18.52  1    4    2
#> Toyota Corolla 33.9  71.1  65 4.22 1.835 19.90  1    4    1
#> Fiat X1-9      27.3  79.0  66 4.08 1.935 18.90  1    4    1
#> Porsche 914-2  26.0 120.3  91 4.43 2.140 16.70  0    5    2
#> Lotus Europa   30.4  95.1 113 3.77 1.513 16.90  1    5    2
#> Volvo 142E     21.4 121.0 109 4.11 2.780 18.60  1    4    2
#> 
#> [[5]]
#>                mpg disp  hp drat    wt  qsec vs gear carb
#> Mazda RX4     21.0  160 110 3.90 2.620 16.46  0    4    4
#> Mazda RX4 Wag 21.0  160 110 3.90 2.875 17.02  0    4    4
#> Ferrari Dino  19.7  145 175 3.62 2.770 15.50  0    5    6
#> 
#> [[6]]
#>                 mpg disp  hp drat   wt qsec vs gear carb
#> Ford Pantera L 15.8  351 264 4.22 3.17 14.5  0    5    4
#> Maserati Bora  15.0  301 335 3.54 3.57 14.6  0    5    8
#> 
mtcars %>% group_by(cyl, am) %>% group_keys()
#>   cyl am
#> 1   4  0
#> 2   4  1
#> 3   6  0
#> 4   6  1
#> 5   8  0
#> 6   8  1

# Ungrouped data.frames:
mtcars %>% group_split(am, cyl)
#> [[1]]
#>                mpg cyl  disp hp drat    wt  qsec vs am gear carb
#> Merc 240D     24.4   4 146.7 62 3.69 3.190 20.00  1  0    4    2
#> Merc 230      22.8   4 140.8 95 3.92 3.150 22.90  1  0    4    2
#> Toyota Corona 21.5   4 120.1 97 3.70 2.465 20.01  1  0    3    1
#> 
#> [[2]]
#>                 mpg cyl  disp  hp drat    wt  qsec vs am gear carb
#> Datsun 710     22.8   4 108.0  93 3.85 2.320 18.61  1  1    4    1
#> Fiat 128       32.4   4  78.7  66 4.08 2.200 19.47  1  1    4    1
#> Honda Civic    30.4   4  75.7  52 4.93 1.615 18.52  1  1    4    2
#> Toyota Corolla 33.9   4  71.1  65 4.22 1.835 19.90  1  1    4    1
#> Fiat X1-9      27.3   4  79.0  66 4.08 1.935 18.90  1  1    4    1
#> Porsche 914-2  26.0   4 120.3  91 4.43 2.140 16.70  0  1    5    2
#> Lotus Europa   30.4   4  95.1 113 3.77 1.513 16.90  1  1    5    2
#> Volvo 142E     21.4   4 121.0 109 4.11 2.780 18.60  1  1    4    2
#> 
#> [[3]]
#>                 mpg cyl  disp  hp drat    wt  qsec vs am gear carb
#> Hornet 4 Drive 21.4   6 258.0 110 3.08 3.215 19.44  1  0    3    1
#> Valiant        18.1   6 225.0 105 2.76 3.460 20.22  1  0    3    1
#> Merc 280       19.2   6 167.6 123 3.92 3.440 18.30  1  0    4    4
#> Merc 280C      17.8   6 167.6 123 3.92 3.440 18.90  1  0    4    4
#> 
#> [[4]]
#>                mpg cyl disp  hp drat    wt  qsec vs am gear carb
#> Mazda RX4     21.0   6  160 110 3.90 2.620 16.46  0  1    4    4
#> Mazda RX4 Wag 21.0   6  160 110 3.90 2.875 17.02  0  1    4    4
#> Ferrari Dino  19.7   6  145 175 3.62 2.770 15.50  0  1    5    6
#> 
#> [[5]]
#>                      mpg cyl  disp  hp drat    wt  qsec vs am gear carb
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
#> 
#> [[6]]
#>                 mpg cyl disp  hp drat   wt qsec vs am gear carb
#> Ford Pantera L 15.8   8  351 264 4.22 3.17 14.5  0  1    5    4
#> Maserati Bora  15.0   8  301 335 3.54 3.57 14.6  0  1    5    8
#> 
```
