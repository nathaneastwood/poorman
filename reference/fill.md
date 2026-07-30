# Fill in missing values with previous or next value

Fills missing values in selected columns using the next or previous
entry. This is useful in the common output format where values are not
repeated, and are only recorded when they change.

## Usage

``` r
fill(data, ..., .direction = c("down", "up", "downup", "updown"))
```

## Arguments

- data:

  A `data.frame`.

- ...:

  Columns to fill.

- .direction:

  Direction in which to fill missing values. Currently either `"down"`
  (the default), `"up"`, `"downup"` (i.e. first down and then up) or
  `"updown"` (first up and then down).

## Details

Missing values are replaced in atomic vectors; `NULL`s are replaced in
lists.

## Examples

``` r
# Value (year) is recorded only when it changes
sales <- data.frame(
  quarter = c(
    "Q1", "Q2", "Q3", "Q4", "Q1", "Q2", "Q3", "Q4", "Q1", "Q2",
    "Q3", "Q4", "Q1", "Q2", "Q3", "Q4"
  ),
  year = c(2000, NA, NA, NA, 2001, NA, NA, NA, 2002, NA, NA, NA, 2004, NA, NA, NA),
  sales = c(
    66013, 69182, 53175, 21001, 46036, 58842, 44568, 50197, 39113, 41668, 30144,
    52897, 32129, 67686, 31768, 49094
  )
)

# `fill()` defaults to replacing missing data from top to bottom
sales %>% fill(year)
#>    quarter year sales
#> 1       Q1 2000 66013
#> 2       Q2 2000 69182
#> 3       Q3 2000 53175
#> 4       Q4 2000 21001
#> 5       Q1 2001 46036
#> 6       Q2 2001 58842
#> 7       Q3 2001 44568
#> 8       Q4 2001 50197
#> 9       Q1 2002 39113
#> 10      Q2 2002 41668
#> 11      Q3 2002 30144
#> 12      Q4 2002 52897
#> 13      Q1 2004 32129
#> 14      Q2 2004 67686
#> 15      Q3 2004 31768
#> 16      Q4 2004 49094

# Value (pet_type) is missing above
tidy_pets <- data.frame(
  rank = c(1L, 2L, 3L, 4L, 5L, 6L, 1L, 2L, 3L, 4L, 5L, 6L),
  pet_type = c(NA, NA, NA, NA, NA, "Dog", NA, NA, NA, NA, NA, "Cat"),
  breed = c(
    "Boston Terrier", "Retrievers (Labrador)", "Retrievers (Golden)",
    "French Bulldogs", "Bulldogs", "Beagles", "Persian", "Maine Coon",
    "Ragdoll", "Exotic", "Siamese", "American Short"
  )
)

# For values that are missing above you can use `.direction = "up"`
tidy_pets %>%
  fill(pet_type, .direction = "up")
#>    rank pet_type                 breed
#> 1     1      Dog        Boston Terrier
#> 2     2      Dog Retrievers (Labrador)
#> 3     3      Dog   Retrievers (Golden)
#> 4     4      Dog       French Bulldogs
#> 5     5      Dog              Bulldogs
#> 6     6      Dog               Beagles
#> 7     1      Cat               Persian
#> 8     2      Cat            Maine Coon
#> 9     3      Cat               Ragdoll
#> 10    4      Cat                Exotic
#> 11    5      Cat               Siamese
#> 12    6      Cat        American Short

# Value (n_squirrels) is missing above and below within a group
squirrels <- data.frame(
  group = c(1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3),
  name = c(
    "Sam", "Mara", "Jesse", "Tom", "Mike", "Rachael", "Sydekea",
    "Gabriela", "Derrick", "Kara", "Emily", "Danielle"
  ),
  role = c(
    "Observer", "Scorekeeper", "Observer", "Observer", "Observer",
    "Observer", "Scorekeeper", "Observer", "Observer", "Scorekeeper",
    "Observer", "Observer"
  ),
  n_squirrels = c(NA, 8, NA, NA, NA, NA, 14, NA, NA, 9, NA, NA)
)

# The values are inconsistently missing by position within the group
# Use .direction = "downup" to fill missing values in both directions
squirrels %>%
  group_by(group) %>%
  fill(n_squirrels, .direction = "downup") %>%
  ungroup()
#> Adding missing grouping variables: `group`
#> Adding missing grouping variables: `group`
#> Adding missing grouping variables: `group`
#>    group     name        role n_squirrels
#> 1      1      Sam    Observer           8
#> 2      1     Mara Scorekeeper           8
#> 3      1    Jesse    Observer           8
#> 4      1      Tom    Observer           8
#> 5      2     Mike    Observer          14
#> 6      2  Rachael    Observer          14
#> 7      2  Sydekea Scorekeeper          14
#> 8      2 Gabriela    Observer          14
#> 9      3  Derrick    Observer           9
#> 10     3     Kara Scorekeeper           9
#> 11     3    Emily    Observer           9
#> 12     3 Danielle    Observer           9

# Using `.direction = "updown"` accomplishes the same goal in this example
```
