# Select Grouping Variables

This selection helper matches grouping variables. It can be used within
[`select()`](select.md) and [`relocate()`](relocate.md) selections.

## Usage

``` r
group_cols()
```

## See also

[`groups()`](group_metadata.md) and [`group_vars()`](group_metadata.md)
for retrieving the grouping variables outside selection contexts.

## Examples

``` r
mtcars %>% group_by(am, cyl) %>% select(group_cols())
#>                     am cyl
#> Mazda RX4            1   6
#> Mazda RX4 Wag        1   6
#> Datsun 710           1   4
#> Hornet 4 Drive       0   6
#> Hornet Sportabout    0   8
#> Valiant              0   6
#> Duster 360           0   8
#> Merc 240D            0   4
#> Merc 230             0   4
#> Merc 280             0   6
#> Merc 280C            0   6
#> Merc 450SE           0   8
#> Merc 450SL           0   8
#> Merc 450SLC          0   8
#> Cadillac Fleetwood   0   8
#> Lincoln Continental  0   8
#> Chrysler Imperial    0   8
#> Fiat 128             1   4
#> Honda Civic          1   4
#> Toyota Corolla       1   4
#> Toyota Corona        0   4
#> Dodge Challenger     0   8
#> AMC Javelin          0   8
#> Camaro Z28           0   8
#> Pontiac Firebird     0   8
#> Fiat X1-9            1   4
#> Porsche 914-2        1   4
#> Lotus Europa         1   4
#> Ford Pantera L       1   8
#> Ferrari Dino         1   6
#> Maserati Bora        1   8
#> Volvo 142E           1   4
```
