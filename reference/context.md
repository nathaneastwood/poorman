# Context dependent expressions

These functions return information about the "current" group or
"current" variable, so only work inside specific contexts like
[`summarise()`](summarise.md) and [`mutate()`](mutate.md).

- `n()` gives the number of observations in the current group.

&nbsp;

- `cur_data()` gives the current data for the current group (excluding
  grouping variables).

&nbsp;

- `cur_data_all()` gives the current data for the current group
  (including grouping variables).

&nbsp;

- `cur_group()` gives the group keys, a single row `data.frame`
  containing a column for each grouping variable and its value.

&nbsp;

- `cur_group_id()` gives a unique numeric identifier for the current
  group.

&nbsp;

- `cur_group_rows()` gives the rows the groups appear in the data.

&nbsp;

- `cur_column()` gives the name of the current column (in
  [`across()`](across.md) only).

## Usage

``` r
n()

cur_data()

cur_data_all()

cur_group()

cur_group_id()

cur_group_rows()

cur_column()
```

## `data.table`

If you're familiar with `data.table`:

- `cur_data()` \<-\> `.SD`

- `cur_group_id()` \<-\> `.GRP`

- `cur_group()` \<-\> `.BY`

- `cur_group_rows()` \<-\> `.I`

## See also

See [`group_data()`](group_metadata.md) for equivalent functions that
return values for all groups.

## Examples

``` r
df <- data.frame(
  g = sample(rep(letters[1:3], 1:3)),
  x = runif(6),
  y = runif(6),
  stringsAsFactors = FALSE
)
gf <- df %>% group_by(g)

gf %>% summarise(n = n())
#>   g n
#> 1 a 1
#> 2 b 2
#> 3 c 3

gf %>% mutate(id = cur_group_id())
#>   g         x          y id
#> 1 c 0.9835254 0.13807484  3
#> 2 c 0.5898376 0.08084496  3
#> 3 c 0.7591584 0.65598263  3
#> 4 b 0.8360753 0.60200386  2
#> 5 b 0.7628195 0.65699583  2
#> 6 a 0.4172699 0.32931716  1
gf %>% summarise(row = cur_group_rows())
#> `summarise()` has grouped output by 'g'. You can override using the `.groups` argument.
#>   g row
#> 1 a   6
#> 2 b   4
#> 3 b   5
#> 4 c   1
#> 5 c   2
#> 6 c   3
gf %>% summarise(data = list(cur_group()))
#>   g data
#> 1 a    a
#> 2 b    b
#> 3 c    c
gf %>% summarise(data = list(cur_data()))
#>   g                                                                   data
#> 1 a                                                   0.4172699, 0.3293172
#> 2 b                             0.8360753, 0.7628195, 0.6020039, 0.6569958
#> 3 c 0.98352541, 0.58983756, 0.75915838, 0.13807484, 0.08084496, 0.65598263
gf %>% summarise(data = list(cur_data_all()))
#>   g
#> 1 a
#> 2 b
#> 3 c
#>                                                                                                                         data
#> 1                                                                                    a, 0.417269926285371, 0.329317163676023
#> 2                                           b, b, 0.836075305938721, 0.762819469673559, 0.602003859123215, 0.656995829660445
#> 3 c, c, c, 0.983525407966226, 0.589837556472048, 0.759158379863948, 0.138074840884656, 0.0808449615724385, 0.655982626136392

gf %>% mutate(across(everything(), ~ paste(cur_column(), round(.x, 2))))
#>   g      x      y
#> 1 c x 0.98 y 0.14
#> 2 c x 0.59 y 0.08
#> 3 c x 0.76 y 0.66
#> 4 b x 0.84  y 0.6
#> 5 b x 0.76 y 0.66
#> 6 a x 0.42 y 0.33
```
