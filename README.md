<!-- README.md is generated from README.Rmd. Please edit that file -->
<h1 align="center">
<code>poorman</code>
</h1>
<blockquote align="center">
I’d seen my father. He was a poor man, and I watched him do astonishing
things. - Sidney Poitier
</blockquote>

Goal
----

The goal of `poorman` is to provide base R versions of `dplyr`
functions. Check out the blog posts
[here](https://nathaneastwood.github.io/tags/poorman/) for more details.

Installation
------------

You can install the released version of `poorman` from
[CRAN](https://CRAN.R-project.org) with:

    install.packages("poorman")

And the development version from [GitHub](https://github.com/) with:

    # install.packages("devtools")
    devtools::install_github("nathaneastwood/poorman")

Features
--------

`poorman` attempts to replicate the `dplyr` API exactly such that your
dplyr code will still run even if you use `poorman` in its place, it
even comes with its own `magrittr` pipe (`%>%`) replica. Below are some
of the operations that are currently available.

    library(poorman)
    # 
    #   I'd seen my father. He was a poor man, and I watched him do astonishing things.
    #     - Sidney Poitier
    # 
    # Attaching package: 'poorman'
    # The following object is masked from 'package:stats':
    # 
    #     filter

### `select()`

    mtcars %>% select(mpg, gear, disp)
    #                      mpg gear  disp
    # Mazda RX4           21.0    4 160.0
    # Mazda RX4 Wag       21.0    4 160.0
    # Datsun 710          22.8    4 108.0
    # Hornet 4 Drive      21.4    3 258.0
    # Hornet Sportabout   18.7    3 360.0
    # Valiant             18.1    3 225.0
    # Duster 360          14.3    3 360.0
    # Merc 240D           24.4    4 146.7
    # Merc 230            22.8    4 140.8
    # Merc 280            19.2    4 167.6
    # Merc 280C           17.8    4 167.6
    # Merc 450SE          16.4    3 275.8
    # Merc 450SL          17.3    3 275.8
    # Merc 450SLC         15.2    3 275.8
    # Cadillac Fleetwood  10.4    3 472.0
    # Lincoln Continental 10.4    3 460.0
    # Chrysler Imperial   14.7    3 440.0
    # Fiat 128            32.4    4  78.7
    # Honda Civic         30.4    4  75.7
    # Toyota Corolla      33.9    4  71.1
    # Toyota Corona       21.5    3 120.1
    # Dodge Challenger    15.5    3 318.0
    # AMC Javelin         15.2    3 304.0
    # Camaro Z28          13.3    3 350.0
    # Pontiac Firebird    19.2    3 400.0
    # Fiat X1-9           27.3    4  79.0
    # Porsche 914-2       26.0    5 120.3
    # Lotus Europa        30.4    5  95.1
    # Ford Pantera L      15.8    5 351.0
    # Ferrari Dino        19.7    5 145.0
    # Maserati Bora       15.0    5 301.0
    # Volvo 142E          21.4    4 121.0

### `filter()`

    mtcars %>% filter(mpg > 28)
    #                 mpg cyl disp  hp drat    wt  qsec vs am gear carb
    # Fiat 128       32.4   4 78.7  66 4.08 2.200 19.47  1  1    4    1
    # Honda Civic    30.4   4 75.7  52 4.93 1.615 18.52  1  1    4    2
    # Toyota Corolla 33.9   4 71.1  65 4.22 1.835 19.90  1  1    4    1
    # Lotus Europa   30.4   4 95.1 113 3.77 1.513 16.90  1  1    5    2

### `mutate()`

    mtcars %>% mutate(mpg2 = mpg ^ 2)
    #                      mpg cyl  disp  hp drat    wt  qsec vs am gear carb    mpg2
    # Mazda RX4           21.0   6 160.0 110 3.90 2.620 16.46  0  1    4    4  441.00
    # Mazda RX4 Wag       21.0   6 160.0 110 3.90 2.875 17.02  0  1    4    4  441.00
    # Datsun 710          22.8   4 108.0  93 3.85 2.320 18.61  1  1    4    1  519.84
    # Hornet 4 Drive      21.4   6 258.0 110 3.08 3.215 19.44  1  0    3    1  457.96
    # Hornet Sportabout   18.7   8 360.0 175 3.15 3.440 17.02  0  0    3    2  349.69
    # Valiant             18.1   6 225.0 105 2.76 3.460 20.22  1  0    3    1  327.61
    # Duster 360          14.3   8 360.0 245 3.21 3.570 15.84  0  0    3    4  204.49
    # Merc 240D           24.4   4 146.7  62 3.69 3.190 20.00  1  0    4    2  595.36
    # Merc 230            22.8   4 140.8  95 3.92 3.150 22.90  1  0    4    2  519.84
    # Merc 280            19.2   6 167.6 123 3.92 3.440 18.30  1  0    4    4  368.64
    # Merc 280C           17.8   6 167.6 123 3.92 3.440 18.90  1  0    4    4  316.84
    # Merc 450SE          16.4   8 275.8 180 3.07 4.070 17.40  0  0    3    3  268.96
    # Merc 450SL          17.3   8 275.8 180 3.07 3.730 17.60  0  0    3    3  299.29
    # Merc 450SLC         15.2   8 275.8 180 3.07 3.780 18.00  0  0    3    3  231.04
    # Cadillac Fleetwood  10.4   8 472.0 205 2.93 5.250 17.98  0  0    3    4  108.16
    # Lincoln Continental 10.4   8 460.0 215 3.00 5.424 17.82  0  0    3    4  108.16
    # Chrysler Imperial   14.7   8 440.0 230 3.23 5.345 17.42  0  0    3    4  216.09
    # Fiat 128            32.4   4  78.7  66 4.08 2.200 19.47  1  1    4    1 1049.76
    # Honda Civic         30.4   4  75.7  52 4.93 1.615 18.52  1  1    4    2  924.16
    # Toyota Corolla      33.9   4  71.1  65 4.22 1.835 19.90  1  1    4    1 1149.21
    # Toyota Corona       21.5   4 120.1  97 3.70 2.465 20.01  1  0    3    1  462.25
    # Dodge Challenger    15.5   8 318.0 150 2.76 3.520 16.87  0  0    3    2  240.25
    # AMC Javelin         15.2   8 304.0 150 3.15 3.435 17.30  0  0    3    2  231.04
    # Camaro Z28          13.3   8 350.0 245 3.73 3.840 15.41  0  0    3    4  176.89
    # Pontiac Firebird    19.2   8 400.0 175 3.08 3.845 17.05  0  0    3    2  368.64
    # Fiat X1-9           27.3   4  79.0  66 4.08 1.935 18.90  1  1    4    1  745.29
    # Porsche 914-2       26.0   4 120.3  91 4.43 2.140 16.70  0  1    5    2  676.00
    # Lotus Europa        30.4   4  95.1 113 3.77 1.513 16.90  1  1    5    2  924.16
    # Ford Pantera L      15.8   8 351.0 264 4.22 3.170 14.50  0  1    5    4  249.64
    # Ferrari Dino        19.7   6 145.0 175 3.62 2.770 15.50  0  1    5    6  388.09
    # Maserati Bora       15.0   8 301.0 335 3.54 3.570 14.60  0  1    5    8  225.00
    # Volvo 142E          21.4   4 121.0 109 4.11 2.780 18.60  1  1    4    2  457.96

### `arrange()`

    mtcars %>% arrange(mpg)
    #                      mpg cyl  disp  hp drat    wt  qsec vs am gear carb
    # Cadillac Fleetwood  10.4   8 472.0 205 2.93 5.250 17.98  0  0    3    4
    # Lincoln Continental 10.4   8 460.0 215 3.00 5.424 17.82  0  0    3    4
    # Camaro Z28          13.3   8 350.0 245 3.73 3.840 15.41  0  0    3    4
    # Duster 360          14.3   8 360.0 245 3.21 3.570 15.84  0  0    3    4
    # Chrysler Imperial   14.7   8 440.0 230 3.23 5.345 17.42  0  0    3    4
    # Maserati Bora       15.0   8 301.0 335 3.54 3.570 14.60  0  1    5    8
    # Merc 450SLC         15.2   8 275.8 180 3.07 3.780 18.00  0  0    3    3
    # AMC Javelin         15.2   8 304.0 150 3.15 3.435 17.30  0  0    3    2
    # Dodge Challenger    15.5   8 318.0 150 2.76 3.520 16.87  0  0    3    2
    # Ford Pantera L      15.8   8 351.0 264 4.22 3.170 14.50  0  1    5    4
    # Merc 450SE          16.4   8 275.8 180 3.07 4.070 17.40  0  0    3    3
    # Merc 450SL          17.3   8 275.8 180 3.07 3.730 17.60  0  0    3    3
    # Merc 280C           17.8   6 167.6 123 3.92 3.440 18.90  1  0    4    4
    # Valiant             18.1   6 225.0 105 2.76 3.460 20.22  1  0    3    1
    # Hornet Sportabout   18.7   8 360.0 175 3.15 3.440 17.02  0  0    3    2
    # Merc 280            19.2   6 167.6 123 3.92 3.440 18.30  1  0    4    4
    # Pontiac Firebird    19.2   8 400.0 175 3.08 3.845 17.05  0  0    3    2
    # Ferrari Dino        19.7   6 145.0 175 3.62 2.770 15.50  0  1    5    6
    # Mazda RX4           21.0   6 160.0 110 3.90 2.620 16.46  0  1    4    4
    # Mazda RX4 Wag       21.0   6 160.0 110 3.90 2.875 17.02  0  1    4    4
    # Hornet 4 Drive      21.4   6 258.0 110 3.08 3.215 19.44  1  0    3    1
    # Volvo 142E          21.4   4 121.0 109 4.11 2.780 18.60  1  1    4    2
    # Toyota Corona       21.5   4 120.1  97 3.70 2.465 20.01  1  0    3    1
    # Datsun 710          22.8   4 108.0  93 3.85 2.320 18.61  1  1    4    1
    # Merc 230            22.8   4 140.8  95 3.92 3.150 22.90  1  0    4    2
    # Merc 240D           24.4   4 146.7  62 3.69 3.190 20.00  1  0    4    2
    # Porsche 914-2       26.0   4 120.3  91 4.43 2.140 16.70  0  1    5    2
    # Fiat X1-9           27.3   4  79.0  66 4.08 1.935 18.90  1  1    4    1
    # Honda Civic         30.4   4  75.7  52 4.93 1.615 18.52  1  1    4    2
    # Lotus Europa        30.4   4  95.1 113 3.77 1.513 16.90  1  1    5    2
    # Fiat 128            32.4   4  78.7  66 4.08 2.200 19.47  1  1    4    1
    # Toyota Corolla      33.9   4  71.1  65 4.22 1.835 19.90  1  1    4    1

### `group_by()`

    mtcars %>%
      group_by(am, cyl) %>%
      summarise(meanMpg = mean(mpg), sumMpg = sum(mpg))
    #   am cyl  meanMpg sumMpg
    # 1  0   4     22.9   68.7
    # 2  1   4   28.075  224.6
    # 3  0   6   19.125   76.5
    # 4  1   6 20.56667   61.7
    # 5  0   8    15.05  180.6
    # 6  1   8     15.4   30.8

Related Work
------------

[`bplyr`](https://github.com/yonicd/bplyr) and
[`tbltools`](https://github.com/mkearney/tbltools) both attempt to offer
similar functionality to this package. Note however that `bplyr` imports
`magrittr` and uses `rlang` under the hood and `tbltools` imports
`magrittr` and appends `_data` to each of its functions,
e.g. `select_data()`. `poorman` attempts to replicate the `dplyr` API
exactly without additional packages.
