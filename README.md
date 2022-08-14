<!-- README.md is generated from README.Rmd. Please edit that file -->

# {poorman} <a href='https://nathaneastwood.github.io/tags/poorman/'><img src='man/figures/logo.png' align="right" height="139" /></a>

[![CRAN
status](https://www.r-pkg.org/badges/version/poorman)](https://cran.r-project.org/package=poorman)
[![Dependencies](https://tinyverse.netlify.com/badge/poorman)](https://cran.r-project.org/package=poorman)
![CRAN downloads](https://cranlogs.r-pkg.org/badges/poorman)
![check_cran](https://github.com/nathaneastwood/poorman/workflows/check_cran/badge.svg?branch=master)
[![codecov](https://codecov.io/gh/nathaneastwood/poorman/branch/master/graph/badge.svg?token=YPQSSEEHZJ)](https://codecov.io/gh/nathaneastwood/poorman)

<blockquote align="center">
I’d seen my father. He was a poor man, and I watched him do astonishing
things. - Sidney Poitier
</blockquote>

## Overview

{poorman} is a grammar of data manipulation, providing dependency free
versions of [{dplyr}](https://github.com/tidyverse/dplyr) verbs that
help you solve the most common data manipulation challenges:

-   `select()` picks variables based on their names.
-   `mutate()` adds new variables that are functions of existing
    variables.
-   `filter()` picks cases based on their values.
-   `summarise()` reduces multiple values down to a single summary.
-   `arrange()` changes the ordering of the rows.

{poorman} attempts to replicate the {dplyr} API exactly such that your
{dplyr} code will still run even if you use {poorman} in its place. In
addition to replicating {dplyr} functionality, {poorman} implements
other functionality from the wider {tidyverse} such as select helpers
and the pipe, `%>%`.

For more details on the functionality available within {poorman}, check
out the {poorman} series of blog posts
[here](https://nathaneastwood.github.io/tags/poorman/).

## Installation

You can install:

-   the development version from
    [GitHub](https://github.com/nathaneastwood/poorman) with

``` r
# install.packages("remotes")
remotes::install_github("nathaneastwood/poorman")
```

-   the latest release from CRAN with

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
  select(mpg, wt, starts_with("c")) %>%
  mutate(kpl = (1.609 * mpg) / 3.785, wt_kg = wt * 453.5924) %>%
  filter(mpg > 28)
#                 mpg    wt cyl carb      kpl    wt_kg
# Fiat 128       32.4 2.200   4    1 13.77321 997.9033
# Honda Civic    30.4 1.615   4    2 12.92301 732.5517
# Toyota Corolla 33.9 1.835   4    1 14.41086 832.3421
# Lotus Europa   30.4 1.513   4    2 12.92301 686.2853

mtcars %>%
  group_by(am, cyl) %>%
  summarise(mean_mpg = mean(mpg), sd_mpg = sd(mpg)) %>%
  ungroup()
#   am cyl mean_mpg    sd_mpg
# 1  0   4 22.90000 1.4525839
# 2  0   6 19.12500 1.6317169
# 3  0   8 15.05000 2.7743959
# 4  1   4 28.07500 4.4838599
# 5  1   6 20.56667 0.7505553
# 6  1   8 15.40000 0.5656854
```

## Related Work

-   [{dplyr}](https://github.com/tidyverse/dplyr)
-   [{bplyr}](https://github.com/yonicd/bplyr) - imports {magrittr} and
    {rlang}; it prepends functions with `b_*()`, e.g. `b_select()`.
-   [{tbltools}](https://github.com/mkearney/tbltools) - imports
    {magrittr} and appends `*_data()` to each of its functions,
    e.g. `select_data()`.
