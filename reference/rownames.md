# Tools for working with row names

In some quarters, it is considered best to avoid row names, because they
are effectively a character column with different semantics than every
other column. These functions allow to you detect if a `data.frame` has
row names (`has_rownames()`), remove them (`remove_rownames()`), or
convert them back-and-forth between an explicit column
(`rownames_to_column()` and `column_to_rownames()`). Also included is
`rowid_to_column()`, which adds a column at the start of the dataframe
of ascending sequential row ids starting at 1. Note that this will
remove any existing row names.

## Usage

``` r
rownames_to_column(.data, var = "rowname")

rowid_to_column(.data, var = "rowid")

column_to_rownames(.data, var = "rowname")

remove_rownames(.data)

has_rownames(.data)
```

## Arguments

- .data:

  A `data.frame`.

- var:

  `character(1)`. The name of the column to use for row names.

## Value

- `column_to_rownames()` always returns a `data.frame`.

- `has_rownames()` returns a `logical(1)`.

- All other functions return an object of the same class as the input.

## Examples

``` r
# Detect row names
has_rownames(mtcars)
#> [1] TRUE
has_rownames(iris)
#> [1] FALSE

# Remove row names
remove_rownames(mtcars) %>% has_rownames()
#> [1] FALSE

# Convert between row names and column
mtcars <- rownames_to_column(mtcars, var = "car")
column_to_rownames(mtcars, var = "car") %>% head()
#>                    mpg cyl disp  hp drat    wt  qsec vs am gear carb
#> Mazda RX4         21.0   6  160 110 3.90 2.620 16.46  0  1    4    4
#> Mazda RX4 Wag     21.0   6  160 110 3.90 2.875 17.02  0  1    4    4
#> Datsun 710        22.8   4  108  93 3.85 2.320 18.61  1  1    4    1
#> Hornet 4 Drive    21.4   6  258 110 3.08 3.215 19.44  1  0    3    1
#> Hornet Sportabout 18.7   8  360 175 3.15 3.440 17.02  0  0    3    2
#> Valiant           18.1   6  225 105 2.76 3.460 20.22  1  0    3    1

# Adding rowid as a column
rowid_to_column(iris) %>% head()
#>   rowid Sepal.Length Sepal.Width Petal.Length Petal.Width Species
#> 1     1          5.1         3.5          1.4         0.2  setosa
#> 2     2          4.9         3.0          1.4         0.2  setosa
#> 3     3          4.7         3.2          1.3         0.2  setosa
#> 4     4          4.6         3.1          1.5         0.2  setosa
#> 5     5          5.0         3.6          1.4         0.2  setosa
#> 6     6          5.4         3.9          1.7         0.4  setosa
```
