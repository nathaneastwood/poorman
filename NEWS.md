# poorman 0.2.6 (devel)

* The `.names` argument in `across()` now accepts `{.col}` and `{.fn}` to 
automatically name new columns (#100, @etiennebacher).
* `arrange()` now works for descending character vectors (#99, @etiennebacher).

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
