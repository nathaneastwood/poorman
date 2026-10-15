# poorman 0.2.8

## Breaking changes

* The arguments of `pivot_wider()` are now in the same order as in `tidyr` 1.1: `data`, `id_cols`, `names_from`, `names_prefix`, `names_sep`, `names_glue`, `names_sort`, `names_repair`, `values_from`, `values_fill`, `values_fn`. Code which passes `values_from` or `names_from` by position must now name them.

## New features

* `arrange()` now supports `across()` (#88).
* `arrange()` now supports list, data frame and matrix columns. Data frame and matrix columns are sorted by each of their columns in turn and list columns are sorted by the first appearance of each element, as in `dplyr` (#90).

## Improvements

Grouped operations are now considerably faster. Groups are identified in a single pass over the data rather than by comparing every group against every row, so the cost of `group_by()` no longer grows with the number of groups. On 100,000 rows with 1,000 groups, `group_by()` and `arrange()` are over 100x faster, `filter()`, `mutate()` and `count()` 7-20x faster, `group_indices()` over 1,000x faster, and grouping by several high cardinality columns no longer exhausts memory.

`pivot_longer()` and `pivot_wider()` have been rewritten without `stats::reshape()` and are now considerably faster and use far less memory. On 100,000 rows by 10 columns, `pivot_longer()` is over 35x faster; on 1,000,000 rows, `pivot_wider()` is over 15x faster, and widening to 1,000 columns is over 150x faster and no longer allocates 3.5GB. Both functions now follow `tidyr` 1.1.

`summarise()` is now considerably faster on grouped data. Each column is split by group once and the summaries are combined column by column, rather than building and binding a `data.frame` for each group. On 100,000 rows, `summarise()` is over 4x faster with 1,000 groups and 5x faster with 10,000 groups, and `count()` with 26,000 groups is over 7x faster. `across()` builds its result faster, which also speeds up `mutate()`.

## Bug fixes

Grouped operations now match `dplyr`:

* Groups whose values paste to the same string (e.g. `"a.b"` and `"c"` vs `"a"` and `"b.c"`) are no longer merged; the same applies to the keys of `semi_join()` and `anti_join()`.
* Rows with missing values in grouping columns are no longer dropped by grouped verbs.
* Numeric grouping values are compared exactly.
* `group_by(.drop = FALSE)` now includes every combination of factor levels and preserves ordered factors.
* `group_split()` now returns groups in the same order as `group_keys()`, and both include empty groups.
* `group_indices()` and `cur_group_id()` now count empty groups.
* `group_data()` is now correct after a grouped `mutate()`, `transmute()` or `fill()`.
* `filter()`, `arrange()` and `summarise()` now respect zero-length groups, and `summarise(.groups = "drop")` returns an ungrouped `data.frame`.
* Grouped verbs now accept new columns named `f`, `.fn`, `.drop` or `.reorder`.
* Grouped verbs now work with zero-row data, and `filter()` now works with single column data.
* `cur_group_id()` now returns `1` for ungrouped data.
* `summarise()` now ignores `NULL` results, as in `dplyr` 1.0.4, rather than failing.
* `summarise()` without any columns on ungrouped data now returns one row, as in `dplyr`.
* `tally()` and `count()` no longer report the grouping of their summary. `tally()` now drops only the last grouping variable, and `count()` now keeps every grouping variable of its input and its `.drop` setting rather than only the first grouping variable.
* `filter()` now combines the columns of a data frame result, e.g. from `across()`, with `&` and warns that this is deprecated, as in `dplyr` 1.0.8. Previously, it used the data frame as a matrix index and returned rows of `NA`. Matrix results must have one column.
* `filter()` now accepts a logical column on its own, e.g. `filter(df, is_valid)`, and gives an error for results which are not logical rather than silently using them.
* `summarise()` now drops the last group when there is only one grouping variable, so the result is ungrouped. Previously, empty summaries, list results, zero row data and `.drop = FALSE` groups kept the grouping.

`summarise()` now matches `dplyr`:

* `summarise()` now evaluates expressions in the calling environment, so local variables are found, for example inside a function. Previously, variables such as `i`, `fns` or `group` were silently taken from `summarise()` itself and poorman's internal objects could mask the user's. As in `dplyr` and `mutate()`, `n()` and the other context functions must now be visible from the calling environment.
* A result which overwrites a column, e.g. `summarise(df, x = mean(x), y = x + 1)`, is now used by later expressions, and the columns of an unnamed data frame result are now available to later expressions.
* Named vector results, e.g. `q = quantile(x, 0.5)`, are now named `q` rather than `x`. Unnamed expressions among named ones are now named after the expression rather than `""`, long expressions are named as `rlang::as_label()` does, e.g. `mean(...)`, and unnamed list results are named after the expression.
* Duplicated names now keep the last result rather than giving duplicated columns, and a result named as a grouping column now replaces it.
* Groups with zero length results now give no rows rather than an error, and length one results are recycled to the size of the group. Results of incompatible sizes and `NULL` results in only some groups now give an informative error.
* Named data frame results, including named `across()` results, are now kept as data frame columns and matrix results as matrix columns, rather than being spread over several columns.
* `summarise()` can now be used inside another `summarise()`, e.g. on `cur_data()`.
* `summarise()` on grouped data with no groups now drops the last grouping variable, as every group has one row.
* `cur_group()` now gives the keys of empty groups rather than `NA`.

Selections and pivots now match `tidyr`:

* Selections which evaluate to column names, such as `names(df)[-1]` or a function returning column names, now work in `pivot_longer()`, `pivot_wider()`, `rename_with()`, `relocate()` and `with_groups()`, and selections are evaluated in the caller's environment so local variables are found (#118).
* `pivot_longer()` now matches `tidyr`:
  * Data with character row names, e.g. `mtcars`, can be pivoted and rows are kept in their input order.
  * Zero row data, tibbles and grouped data are supported; the class and groups of the input are kept.
  * `names_to` supports `".value"` and `NA`, `names_prefix` works with several `names_to` and a single `names_to` uses the matching group of `names_pattern`.
  * The pivoted columns are combined following the `tidyr` type rules, so dates and factors are kept and incompatible types are an error rather than being silently coerced.
  * Attributes of the id columns are kept.
  * Invalid combinations of `names_to`, `names_sep`, `names_pattern` and `values_to` are now errors.
* `pivot_longer()` gains the `tidyr` 1.1 arguments `names_ptypes`, `names_transform`, `names_repair`, `values_ptypes` and `values_transform`, supports `names_to = character()` and keeps every value when the new columns do not uniquely identify each pivoted column, e.g. with duplicated column names or `NA` in `names_to`.
* `pivot_wider()` now matches `tidyr`:
  * `id_cols` accepts any selection, columns which are not selected are dropped and the id columns are returned in the order selected.
  * Factor, date and other classed values keep their class and attributes.
  * `values_fill` only fills combinations which are missing from the data rather than all `NA`s, works with logical columns and with `names_prefix`, keeps integer columns as integer and can be a named list.
  * Values which are not uniquely identified now produce list-columns and a warning rather than silently keeping the first value.
  * With several `values_from` columns, `names_prefix` is added after the value name, e.g. `value_prefix_name`.
  * `names_glue` now works with a single `names_from` column and can use `.value`.
  * Zero row data and tibbles are supported; the class and groups of the input are kept.
* `pivot_wider()` gains the `tidyr` 1.1 arguments `names_sort`, `names_repair` and `values_fn`. The warning for values which are not uniquely identified now matches `tidyr` 1.1, `values_fill` can be `NA` and a `values_fill` which would lose precision is an error.

`arrange()` now matches `dplyr`:

* `arrange()` now evaluates each expression as its own sort key. Previously, it matched expressions to the columns that they used by position, so `arrange(df, x + y, desc(a))` failed when `a` was a character column, and expressions without a column, such as `arrange(df, n():1)`, failed.
* `arrange()` now only treats a top level `desc()` as a sort direction. Previously, it rewrote `desc()` as text, so `arrange(df, desc(x + y))` silently sorted by `-x + y`, and expressions such as `desc(x) + 0` and long expressions failed.
* `arrange()` now finds variables in the calling environment, for example inside a function, and supports the `.data` pronoun there.
* `arrange()` now supports `desc()` on `Date` and other classed columns which do not support unary minus.

# poorman 0.2.7

Eleventh CRAN release. This release:

* Adds a new function `fill()` that mimics `tidyr::fill()` (#113, @etiennebacher).
* Includes various performance improvements for grouped operations, `pivot_wider()` and `pivot_longer()` (@etiennebacher).
* Fixes CRAN issues.

# poorman 0.2.6

Tenth CRAN release.

This update adds the following features:

* Initial versions of `pivot_longer()` and `pivot_wider()` have been added (#101, @etiennebacher).
* The `.names` argument in `across()` now accepts `{.col}` and `{.fn}` to automatically name new columns (#100, @etiennebacher).
* New function `lst()` to reproduce the outputs of `tibble::lst()`. It enables the creation of lists where elements are build sequentially (i.e depend on the previous ones) and are self-named (i.e the expression is used if no name is given) (#108).

The following fixes were also implemented:

* `arrange()` now works for descending character vectors (#99, @etiennebacher).
* `arrange()` now resets row names when they are numeric (#102, @etiennebacher).
* `distinct()` now resets row names when they are numeric (#106, @etiennebacher).

The CICD pipeline was also fixed such that users can now tests pull requests. The documentation is now built using HTML5.

# poorman 0.2.5

Ninth CRAN release.

This update has added the following new features:

* `summarise()` can now revise grouping structures with the `.groups` argument (#57).
* `group_by()` can now drop groups formed by factor levels that don't appear in the data with the `.drop` parameter (#63).
* New cumulative functions have been added: `cumall()`, `cumany()` and `cummean()` (#64).
* Additional tools for working with rownames have been added: `has_rownames()`, `remove_rownames()`, `column_to_rownames()`, `rowid_to_column()` (#66).
* `filter()` gains the `.preserve` parameters (#81).
* `if_any()` and `if_all()` were added (#80).

The following fixes were also implemented:

* `mutate()` now successfully evaluates functions defined in the scope of a parent function (#68).
* `select_positions()` now works if passed a combination of positive and negative values (#71).
* `transmute()` now keeps unnamed columns that it creates (#72).

The following refactorisations took place:

* When grouping data with `group_by()`, it will now create an object of class `grouped_df` and not `grouped_data` which keeps it consistent with the {dplyr} method name (#70). This should hopefully make transitioning packages from {dplyr} to {poorman} a little easier.

# poorman 0.2.4

Eighth CRAN release.

This update has added the following new features:

* `group_by()` can now create new columns, grouping by the result (#58).
* `with_groups()` (#61).
* `nth()` (#62).
* `mutate()` can now relocate resulting mutations (#59).
* `mutate()` can now keep all, used, unused or none of the resulting mutations (#60).
* `across()` can now accept lambda style functions (#52).
* Columns created by `summarise()` are now immediately available (#20).
* `cur_column()` (#52).

The following fixes were also implemented:

* `across()` can now accept function names as characters (#56) - thanks @Eyayaw for pointing this out.
* An empty `summarise()` will now correctly return an empty `data.frame` (#55).

Other general refactorisations and improvements of note:

* `group_by()` is now an S3 generic.
* `.default()` methods are now `.data.frame()` methods.
* `get_groups()` has been removed in favour of `group_vars()`.
* `set_groups()` has been replaced with `groups_set()` for API consistency.

# poorman 0.2.3

Seventh CRAN release.

This update has added the following new features:

* `across()` (#37).
* `na_matches` and `keep` parameters have been added to mutate joins.
* list columns can now be mutated.
* Added the missing ability to use `&` in `poor-select`.

Fixes:

* Use of variables in `relocate()`.

In addition there have been lots of improvements to documentation and many, many more tests added.

# poorman 0.2.2

Sixth CRAN release.

This update has added the following new features:

* `rename_with()`
* `group_cols()` (#42)
* `cur_data_all()`
* `nest_by()` (#44)
* `case_when()` (#23)
* `bind_cols()` (#32)
* `bind_rows()` (#33)
* `unite()`

Performance upgrades and code improvements:

* `pull()` is now much faster thanks to @markfairbanks.
* `%>%` now allows you to pipe into a `.`; my thanks go to @moodymudskipper.

The following bugs have been squashed:

* Filter joins will no longer reduce to vectors thanks to @msberends.
* `contains()` no longer returns lists.
* `relocate()` can now use "tidy select" style column selections.

New documentation components have been added:

* An FAQ vignette has been added (#31).
* A `{pkgdown}` website has been added (#30).

# poorman 0.2.1

Fifth CRAN release.

The update has added the following new features:

* `distinct()` (#17)
* `slice_head()`, `slice_tail()`, `slice_min()`, `slice_max()`, `slice_sample()` (#22)
* `where()` (#27)
* `coalesce()` (#28)
* `group_split()`, `group_keys()` (#29)
* `na_if()` (#34)
* `recode()` (#35)
* `near()` (#36)
* `replace_na()` (#38)
* `group_data()`, `group_indices()`, `group_vars()`, `group_rows()`, `group_size()`, `n_groups()`, `groups()` (#39)
* `glimpse()` (#40)
* `cur_data()`, `cur_group()`, `cur_group_id()`, `cur_group_rows()` (#41)

There have also been a lot of refactoring for the internals of the package, particularly focusing on context awareness which allows for quite a few of these features (#40, #41) and grouped operations should now be more consistent. This update of course comes with a host of bug fixes including the renaming of multiple columns when using `{poorman}`'s version of `{tidyselect}`.

# poorman 0.2.0

Fourth CRAN release.

This update has added the following new features:

* `if_else()` (#11)
* `between()` (#12)
* `lead()` and `lag()` (#13)
* `n_distinct()` (#16)
* window rank functions: `cume_dist()`, `dense_rank()`, `min_rank()`, `ntile()`, `percent_rank()`, `row_number()` (#18)

My thanks go to @msberends for their help on these features!

This update also refactors a lot of the code, hence the minor version bump. This includes:

* More support for "tidyselect" features such as `!`, `:`, `-`, `c()` and `()`
* Removal of `eval(parse(text = ...)`
* Rework of grouped summaries

Finally, 0.2.0 includes many bug fixes:

* Joins now maintain their original order (#10)
* `group_by()` + `mutate()` now preserve row order (#14)
* Quote names are allowed in `pull()` (#15)

# poorman 0.1.11

Third CRAN release.

This update adds the functions `count()`, `tally()`, `add_count()` and `add_tally()`. In order to achieve these, the functions `n()` and `desc()` have also been added. In addition, `group_by()` can now add additional groups to an already grouped `data.frame`.

# poorman 0.1.10

Second CRAN release.

This update adds selection helper functions. See `?select_helpers` for more information.

The following bugs have been fixed:

* `mutate()` column creations are immediately available, e.g. `mtcars %>% mutate(mpg2 = mpg * 2, mpg4 = mpg2 * 2)` will create columns named `mpg2` and `mpg4`
* `group_by()` groups now persist in selections, e.g. `mtcars %>% group_by(am) %>% select(mpg)` will return `am` and `mpg` columns
* `slice()` now duplicates rows, e.g. `mtcars %>% slice(2, 2, 2)` will return row 2 three times
* `summarize()` is now exported

# poorman 0.1.9

First CRAN release

# poorman 0.1.8

This update adds `relocate()` and consolidates the selection method for `select()`, `rename()` and `relocate()` using `select_positions()`.

# poorman 0.1.7

This update adds `semi_join()` and `anti_join()`.

# poorman 0.1.6

This update adds `inner_join()`, `left_join()`, `right_join()` and `full_join()`.

# poorman 0.1.5

This update adds a full set of tests for the functions available in v0.1.5.

# poorman 0.1.4

This update adds a copy of the pipe (`%>%`).

# poorman 0.1.3

This update adds `summarise()`.

# poorman 0.1.2

This update adds `rename()`.

# poorman 0.1.1

This update includes `group_by()` and `ungroup()` operations.

# poorman 0.1.0

This initial version includes the `select()`, `pull()`, `arrange()`, `filter()`, `slice()`, `mutate()` and `transmute()` functions. See the [blog post](https://nathaneastwood.github.io/2020/02/15/building-a-base-dplyr-with-primitives/) for more details.
