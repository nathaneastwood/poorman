# poorman 0.2.2

Sixth CRAN release.

## Documentation changes

This release sees a major overhaul of the package documentation focusing on three components:

* R function documentation is now more focused and fully fleshed out.
* Vignettes have been added to showcase the major components of the package (#31).
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
