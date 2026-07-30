# Recode values

This is a vectorised version of
[`switch()`](https://rdrr.io/r/base/switch.html): you can replace
`numeric` values based on their position or their name, and `character`
or `factor` values only by their name. This is an S3 generic:
`{poorman}` provides methods for `numeric`, `character`, and `factor`s.
For `logical` vectors, use [`if_else()`](if_else.md). For more
complicated criteria, use [`case_when()`](case_when.md).

You can use `recode()` directly with `factor`s; it will preserve the
existing order of levels while changing the values. Alternatively, you
can use `recode_factor()`, which will change the order of levels to
match the order of replacements.

This is a direct port of the `dplyr::recode()` function.

## Usage

``` r
recode(.x, ..., .default = NULL, .missing = NULL)

recode_factor(.x, ..., .default = NULL, .missing = NULL, .ordered = FALSE)
```

## Arguments

- .x:

  A vector to modify

- ...:

  Replacements. For `character` and `factor` `.x`, these should be named
  and replacement is based only on their name. For `numeric` `.x`, these
  can be named or not. If not named, the replacement is done based on
  position i.e. `.x` represents positions to look for in replacements.
  See examples.

  When named, the argument names should be the current values to be
  replaced, and the argument values should be the new (replacement)
  values.

  All replacements must be the same type, and must have either length
  one or the same length as `.x`.

- .default:

  If supplied, all values not otherwise matched will be given this
  value. If not supplied and if the replacements are the same type as
  the original values in `.x`, unmatched values are not changed. If not
  supplied and if the replacements are not compatible, unmatched values
  are replaced with `NA`.

  `.default` must be either length 1 or the same length as `.x`.

- .missing:

  If supplied, any missing values in `.x` will be replaced by this
  value. Must be either length 1 or the same length as `.x`.

- .ordered:

  `logical(1)`. If `TRUE`, `recode_factor()` creates an ordered
  `factor`.

## Value

A vector the same length as `.x`, and the same type as the first of
`...`, `.default`, or `.missing`. `recode_factor()` returns a factor
whose levels are in the same order as in `...`. The levels in `.default`
and `.missing` come last.

## See also

[`na_if()`](na_if.md) to replace specified values with a `NA`.

[`coalesce()`](coalesce.md) to replace missing values with a specified
value.

[`replace_na()`](replace_na.md) to replace `NA` with a value.

## Examples

``` r
# For character values, recode values with named arguments only. Unmatched
# values are unchanged.
char_vec <- sample(c("a", "b", "c"), 10, replace = TRUE)
recode(char_vec, a = "Apple")
#>  [1] "b"     "c"     "c"     "Apple" "b"     "Apple" "c"     "Apple" "Apple"
#> [10] "b"    
recode(char_vec, a = "Apple", b = "Banana")
#>  [1] "Banana" "c"      "c"      "Apple"  "Banana" "Apple"  "c"      "Apple" 
#>  [9] "Apple"  "Banana"

# Use .default as replacement for unmatched values. Note that NA and
# replacement values need to be of the same type.
recode(char_vec, a = "Apple", b = "Banana", .default = NA_character_)
#>  [1] "Banana" NA       NA       "Apple"  "Banana" "Apple"  NA       "Apple" 
#>  [9] "Apple"  "Banana"

# Throws an error as NA is logical, not character.
if (FALSE) { # \dontrun{
recode(char_vec, a = "Apple", b = "Banana", .default = NA)
} # }

# For numeric values, named arguments can also be used
num_vec <- c(1:4, NA)
recode(num_vec, `2` = 20L, `4` = 40L)
#> [1]  1 20  3 40 NA

# Or if you don't name the arguments, recode() matches by position.
# (Only works for numeric vector)
recode(num_vec, "a", "b", "c", "d")
#> [1] "a" "b" "c" "d" NA 
# .x (position given) looks in (...), then grabs (... value at position)
# so if nothing at position (here 5), it uses .default or NA.
recode(c(1, 5, 3), "a", "b", "c", "d", .default = "nothing")
#> [1] "a"       "nothing" "c"      

# Note that if the replacements are not compatible with .x,
# unmatched values are replaced by NA and a warning is issued.
recode(num_vec, `2` = "b", `4` = "d")
#> Warning: Unreplaced values treated as NA as .x is not compatible. Please specify replacements exhaustively or supply .default
#> [1] NA  "b" NA  "d" NA 
# use .default to change the replacement value
recode(num_vec, "a", "b", "c", .default = "other")
#> [1] "a"     "b"     "c"     "other" NA     
# use .missing to replace missing values in .x
recode(num_vec, "a", "b", "c", .default = "other", .missing = "missing")
#> [1] "a"       "b"       "c"       "other"   "missing"

# For factor values, use only named replacements
# and supply default with levels()
factor_vec <- factor(c("a", "b", "c"))
recode(factor_vec, a = "Apple", .default = levels(factor_vec))
#> [1] Apple b     c    
#> Levels: Apple b c

# Use recode_factor() to create factors with levels ordered as they
# appear in the recode call. The levels in .default and .missing
# come last.
recode_factor(num_vec, `1` = "z", `2` = "y", `3` = "x")
#> Warning: Unreplaced values treated as NA as .x is not compatible. Please specify replacements exhaustively or supply .default
#> [1] z    y    x    <NA> <NA>
#> Levels: z y x
recode_factor(num_vec, `1` = "z", `2` = "y", `3` = "x", .default = "D")
#> [1] z    y    x    D    <NA>
#> Levels: z y x D
recode_factor(num_vec, `1` = "z", `2` = "y", `3` = "x", .default = "D", .missing = "M")
#> [1] z y x D M
#> Levels: z y x D M

# When the input vector is a compatible vector (character vector or
# factor), it is reused as default.
recode_factor(letters[1:3], b = "z", c = "y")
#> [1] a z y
#> Levels: z y a
recode_factor(factor(letters[1:3]), b = "z", c = "y")
#> [1] a z y
#> Levels: z y a
```
