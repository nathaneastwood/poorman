# A General Vetorised `if()`

This function allows you to vectorise multiple [`if_else()`](if_else.md)
statements. It is an R equivalent of the SQL `CASE WHEN` statement. If
no cases match, `NA` is returned.

## Usage

``` r
case_when(...)
```

## Arguments

- ...:

  A sequence of two-sided formulas. The left hand side (LHS) determines
  which values match this case. The right hand side (RHS) provides the
  replacement value.

  The LHS must evaluate to a logical vector. The RHS does not need to be
  logical, but all RHSs must evaluate to the same type of vector.

  Both LHS and RHS may have the same length of either 1 or n. The value
  of n must be consistent across all cases. The case of `n == 0` is
  treated as a variant of `n != 1`.

  `NULL` inputs are ignored.

## Value

A vector of length 1 or n, matching the length of the logical input or
output vectors, with the type (and attributes) of the first RHS.
Inconsistent lengths or types will generate an error.

## Examples

``` r
x <- 1:50
case_when(
  x %% 35 == 0 ~ "fizz buzz",
  x %% 5 == 0 ~ "fizz",
  x %% 7 == 0 ~ "buzz",
  TRUE ~ as.character(x)
)
#>  [1] "1"         "2"         "3"         "4"         "fizz"      "6"        
#>  [7] "buzz"      "8"         "9"         "fizz"      "11"        "12"       
#> [13] "13"        "buzz"      "fizz"      "16"        "17"        "18"       
#> [19] "19"        "fizz"      "buzz"      "22"        "23"        "24"       
#> [25] "fizz"      "26"        "27"        "buzz"      "29"        "fizz"     
#> [31] "31"        "32"        "33"        "34"        "fizz buzz" "36"       
#> [37] "37"        "38"        "39"        "fizz"      "41"        "buzz"     
#> [43] "43"        "44"        "fizz"      "46"        "47"        "48"       
#> [49] "buzz"      "fizz"     

# Like an if statement, the arguments are evaluated in order, so you must
# proceed from the most specific to the most general. This won't work:
case_when(
  TRUE ~ as.character(x),
  x %%  5 == 0 ~ "fizz",
  x %%  7 == 0 ~ "buzz",
  x %% 35 == 0 ~ "fizz buzz"
)
#>  [1] "1"  "2"  "3"  "4"  "5"  "6"  "7"  "8"  "9"  "10" "11" "12" "13" "14" "15"
#> [16] "16" "17" "18" "19" "20" "21" "22" "23" "24" "25" "26" "27" "28" "29" "30"
#> [31] "31" "32" "33" "34" "35" "36" "37" "38" "39" "40" "41" "42" "43" "44" "45"
#> [46] "46" "47" "48" "49" "50"

# If none of the cases match, NA is used:
case_when(
  x %%  5 == 0 ~ "fizz",
  x %%  7 == 0 ~ "buzz",
  x %% 35 == 0 ~ "fizz buzz"
)
#>  [1] NA     NA     NA     NA     "fizz" NA     "buzz" NA     NA     "fizz"
#> [11] NA     NA     NA     "buzz" "fizz" NA     NA     NA     NA     "fizz"
#> [21] "buzz" NA     NA     NA     "fizz" NA     NA     "buzz" NA     "fizz"
#> [31] NA     NA     NA     NA     "fizz" NA     NA     NA     NA     "fizz"
#> [41] NA     "buzz" NA     NA     "fizz" NA     NA     NA     "buzz" "fizz"

# Note that NA values in the vector x do not get special treatment. If you want
# to explicitly handle NA values you can use the `is.na` function:
x[2:4] <- NA_real_
case_when(
  x %% 35 == 0 ~ "fizz buzz",
  x %% 5 == 0 ~ "fizz",
  x %% 7 == 0 ~ "buzz",
  is.na(x) ~ "nope",
  TRUE ~ as.character(x)
)
#>  [1] "1"         "nope"      "nope"      "nope"      "fizz"      "6"        
#>  [7] "buzz"      "8"         "9"         "fizz"      "11"        "12"       
#> [13] "13"        "buzz"      "fizz"      "16"        "17"        "18"       
#> [19] "19"        "fizz"      "buzz"      "22"        "23"        "24"       
#> [25] "fizz"      "26"        "27"        "buzz"      "29"        "fizz"     
#> [31] "31"        "32"        "33"        "34"        "fizz buzz" "36"       
#> [37] "37"        "38"        "39"        "fizz"      "41"        "buzz"     
#> [43] "43"        "44"        "fizz"      "46"        "47"        "48"       
#> [49] "buzz"      "fizz"     

# All RHS values need to be of the same type. Inconsistent types will throw an error.
# This applies also to NA values used in RHS: NA is logical, use
# typed values like NA_real_, NA_complex, NA_character_, NA_integer_ as appropriate.
case_when(
  x %% 35 == 0 ~ NA_character_,
  x %% 5 == 0 ~ "fizz",
  x %% 7 == 0 ~ "buzz",
  TRUE ~ as.character(x)
)
#>  [1] "1"    NA     NA     NA     "fizz" "6"    "buzz" "8"    "9"    "fizz"
#> [11] "11"   "12"   "13"   "buzz" "fizz" "16"   "17"   "18"   "19"   "fizz"
#> [21] "buzz" "22"   "23"   "24"   "fizz" "26"   "27"   "buzz" "29"   "fizz"
#> [31] "31"   "32"   "33"   "34"   NA     "36"   "37"   "38"   "39"   "fizz"
#> [41] "41"   "buzz" "43"   "44"   "fizz" "46"   "47"   "48"   "buzz" "fizz"
case_when(
  x %% 35 == 0 ~ 35,
  x %% 5 == 0 ~ 5,
  x %% 7 == 0 ~ 7,
  TRUE ~ NA_real_
)
#>  [1] NA NA NA NA  5 NA  7 NA NA  5 NA NA NA  7  5 NA NA NA NA  5  7 NA NA NA  5
#> [26] NA NA  7 NA  5 NA NA NA NA 35 NA NA NA NA  5 NA  7 NA NA  5 NA NA NA  7  5

# case_when() evaluates all RHS expressions, and then constructs its
# result by extracting the selected (via the LHS expressions) parts.
# In particular NaN are produced in this case:
y <- seq(-2, 2, by = .5)
case_when(
  y >= 0 ~ sqrt(y),
  TRUE   ~ y
)
#> Warning: NaNs produced
#> [1] -2.0000000 -1.5000000 -1.0000000 -0.5000000  0.0000000  0.7071068  1.0000000
#> [8]  1.2247449  1.4142136

if (FALSE) { # \dontrun{
case_when(
  x %% 35 == 0 ~ 35,
  x %% 5 == 0 ~ 5,
  x %% 7 == 0 ~ 7,
  TRUE ~ NA
)
} # }

# case_when is particularly useful inside mutate when you want to
# create a new variable that relies on a complex combination of existing
# variables
mtcars %>%
  mutate(
    efficient = case_when(
      mpg > 25 ~ TRUE,
      TRUE ~ FALSE
    )
  )
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
#>                     efficient
#> Mazda RX4               FALSE
#> Mazda RX4 Wag           FALSE
#> Datsun 710              FALSE
#> Hornet 4 Drive          FALSE
#> Hornet Sportabout       FALSE
#> Valiant                 FALSE
#> Duster 360              FALSE
#> Merc 240D               FALSE
#> Merc 230                FALSE
#> Merc 280                FALSE
#> Merc 280C               FALSE
#> Merc 450SE              FALSE
#> Merc 450SL              FALSE
#> Merc 450SLC             FALSE
#> Cadillac Fleetwood      FALSE
#> Lincoln Continental     FALSE
#> Chrysler Imperial       FALSE
#> Fiat 128                 TRUE
#> Honda Civic              TRUE
#> Toyota Corolla           TRUE
#> Toyota Corona           FALSE
#> Dodge Challenger        FALSE
#> AMC Javelin             FALSE
#> Camaro Z28              FALSE
#> Pontiac Firebird        FALSE
#> Fiat X1-9                TRUE
#> Porsche 914-2            TRUE
#> Lotus Europa             TRUE
#> Ford Pantera L          FALSE
#> Ferrari Dino            FALSE
#> Maserati Bora           FALSE
#> Volvo 142E              FALSE
```
