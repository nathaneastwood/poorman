
<!-- README.md is generated from README.Rmd. Please edit that file -->

# poorman <a href='https://nathaneastwood.github.io/tags/poorman/'><img src='man/figures/logo.png' align="right" height="139" /></a>

[![CRAN
status](https://www.r-pkg.org/badges/version/poorman)](https://cran.r-project.org/package=poorman)
![CRAN downloads](https://cranlogs.r-pkg.org/badges/poorman)
![check\_cran](https://github.com/nathaneastwood/poorman/workflows/check_cran/badge.svg?branch=master)
[![codecov](https://codecov.io/gh/nathaneastwood/poorman/branch/master/graph/badge.svg)](https://codecov.io/gh/nathaneastwood/poorman)

<blockquote align="center">

I’d seen my father. He was a poor man, and I watched him do astonishing
things. - Sidney Poitier

</blockquote>

## Overview

`poorman` is a grammar of data manipulation, providing dependency free
versions of [`dplyr`](https://github.com/tidyverse/dplyr) verbs that
help you solve the most common data manipulation challenges:

  - `select()` picks variables based on their names.
  - `mutate()` adds new variables that are functions of existing
    variables.
  - `filter()` picks cases based on their values.
  - `summarise()` reduces multiple values down to a single summary.
  - `arrange()` changes the ordering of the rows.

`poorman` attempts to replicate the `dplyr` API exactly such that your
`dplyr` code will still run even if you use `poorman` in its place. In
addition to replicating `dplyr` functionality, `poorman` implements
other functionality from the wider `tidyverse` such as select helpers
and the pipe, `%>%`.

For more details on the functionality available within `poorman`, check
out the `poorman` series of blog posts
[here](https://nathaneastwood.github.io/tags/poorman/).

## Installation

You can install the development version from
[GitHub](https://github.com/nathaneastwood/poorman) with:

``` r
# install.packages("remotes")
remotes::install_github("nathaneastwood/poorman")
```

Or you can install the latest release from CRAN with:

``` r
install.packages("poorman")
```

## Docker

If you’d like to try out the latest version of the package on CRAN using
Docker, you can run the latest image with:

``` bash
docker run --rm -it nathaneastwood/poorman
```

## Usage

``` r
library(poorman, warn.conflicts = FALSE)
# 
#   I'd seen my father. He was a poor man, and I watched him do astonishing things.
#     - Sidney Poitier

mtcars %>%
  select(mpg, starts_with("c")) %>%
  mutate(mpg2 = mpg * 2, mpg4 = mpg2 * 2) %>%
  filter(mpg > 28)
#                 mpg cyl carb mpg2  mpg4
# Fiat 128       32.4   4    1 64.8 129.6
# Honda Civic    30.4   4    2 60.8 121.6
# Toyota Corolla 33.9   4    1 67.8 135.6
# Lotus Europa   30.4   4    2 60.8 121.6

mtcars %>%
  group_by(am, cyl) %>%
  summarise(meanMpg = mean(mpg), sumMpg = sum(mpg)) %>%
  ungroup()
#   am cyl  meanMpg sumMpg
# 1  0   4 22.90000   68.7
# 2  0   6 19.12500   76.5
# 3  0   8 15.05000  180.6
# 4  1   4 28.07500  224.6
# 5  1   6 20.56667   61.7
# 6  1   8 15.40000   30.8
```

## Related Work

  - [`dplyr`](https://github.com/tidyverse/dplyr)
  - [`bplyr`](https://github.com/yonicd/bplyr) - imports `magrittr` and
    `rlang`; it prepends functions with `b_*()`, e.g. `b_select()`.
  - [`tbltools`](https://github.com/mkearney/tbltools) - imports
    `magrittr` and appends `*_data()` to each of its functions,
    e.g. `select_data()`.
