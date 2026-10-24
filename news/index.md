# Changelog

## poorman 0.2.9

### Breaking changes

- The arguments of [`pivot_wider()`](../reference/pivot_wider.md) are
  now in the same order as in `tidyr` 1.1: `data`, `id_cols`,
  `names_from`, `names_prefix`, `names_sep`, `names_glue`, `names_sort`,
  `names_repair`, `values_from`, `values_fill`, `values_fn`. Code which
  passes `values_from` or `names_from` by position must now name them.
- Joins now give an error when the join keys of `x` and `y` have
  incompatible types, such as integer and character, as in `dplyr`.
  Previously, the keys were silently converted. A natural join of tables
  with no common columns is now also an error; previously, the mutating
  joins returned a cross join and the filtering joins returned or
  dropped every row. Use `by = character()` for a cross join.
- The mutating joins no longer pass `...` to
  [`merge()`](https://rdrr.io/r/base/merge.html); as in `dplyr`,
  arguments in `...` are ignored.
- [`inner_join()`](../reference/mutate_joins.md),
  [`left_join()`](../reference/mutate_joins.md),
  [`right_join()`](../reference/mutate_joins.md) and
  [`full_join()`](../reference/mutate_joins.md) gain a `copy` argument,
  which is ignored, before `suffix`, as in `dplyr`. Code which passes
  `suffix` by position must now name it.

### New features

- [`arrange()`](../reference/arrange.md) now supports
  [`across()`](../reference/across.md)
  ([\#88](https://github.com/nathaneastwood/poorman/issues/88)).
- [`arrange()`](../reference/arrange.md) now supports list, data frame
  and matrix columns. Data frame and matrix columns are sorted by each
  of their columns in turn and list columns are sorted by the first
  appearance of each element, as in `dplyr`
  ([\#90](https://github.com/nathaneastwood/poorman/issues/90)).
- New [`nest_join()`](../reference/nest_join.md) returns all rows and
  columns of `x` with a list column of the matching rows of `y`, as
  `data.frame`s. Rows of `x` with no match get a 0-row `data.frame`
  ([\#78](https://github.com/nathaneastwood/poorman/issues/78)).

### Improvements

[`inner_join()`](../reference/mutate_joins.md),
[`left_join()`](../reference/mutate_joins.md),
[`right_join()`](../reference/mutate_joins.md) and
[`full_join()`](../reference/mutate_joins.md) have been rewritten
without [`merge()`](https://rdrr.io/r/base/merge.html). Rows are matched
by sorting `y` by key once rather than by
[`merge()`](https://rdrr.io/r/base/merge.html), and the result is built
from the matched row numbers. On 100,000 rows, the joins are 13-16x
faster and allocate 2.5x less memory; on 1,000,000 rows, they are 13-20x
faster.

Grouped operations are now considerably faster. Groups are identified in
a single pass over the data rather than by comparing every group against
every row, so the cost of [`group_by()`](../reference/group_by.md) no
longer grows with the number of groups. On 100,000 rows with 1,000
groups, [`group_by()`](../reference/group_by.md) and
[`arrange()`](../reference/arrange.md) are over 100x faster,
[`filter()`](../reference/filter.md),
[`mutate()`](../reference/mutate.md) and
[`count()`](../reference/count.md) 7-20x faster,
[`group_indices()`](../reference/group_metadata.md) over 1,000x faster,
and grouping by several high cardinality columns no longer exhausts
memory.

[`pivot_longer()`](../reference/pivot_longer.md) and
[`pivot_wider()`](../reference/pivot_wider.md) have been rewritten
without [`stats::reshape()`](https://rdrr.io/r/stats/reshape.html) and
are now considerably faster and use far less memory. On 100,000 rows by
10 columns, [`pivot_longer()`](../reference/pivot_longer.md) is over 35x
faster; on 1,000,000 rows,
[`pivot_wider()`](../reference/pivot_wider.md) is over 15x faster, and
widening to 1,000 columns is over 150x faster and no longer allocates
3.5GB. Both functions now follow `tidyr` 1.1.

[`summarise()`](../reference/summarise.md) is now considerably faster on
grouped data. Each column is split by group once and the summaries are
combined column by column, rather than building and binding a
`data.frame` for each group. On 100,000 rows,
[`summarise()`](../reference/summarise.md) is over 4x faster with 1,000
groups and 5x faster with 10,000 groups, and
[`count()`](../reference/count.md) with 26,000 groups is over 7x faster.
[`across()`](../reference/across.md) builds its result faster, which
also speeds up [`mutate()`](../reference/mutate.md).

### Bug fixes

- `keep = TRUE` now works as in `dplyr`. The keys of `x` and `y` are
  both kept, each with `NA` where a row has no match in its table;
  previously, the key of `y` repeated the key of `x`, and `keep = TRUE`
  with a named `by` failed.
  [`inner_join()`](../reference/mutate_joins.md) now supports `keep`
  too.
- The columns of a join are now in the order of `x` followed by `y`, and
  the rows of `y` without a match now come last in their order in `y`,
  as in `dplyr`. Previously, the key columns came first and the rows of
  `y` without a match were sorted by key. Suffixes are now repeated
  until the names are unique, e.g. `k.x.x`.
- A join which renames all of the grouping columns of `x`, e.g. with
  `keep = TRUE`, now returns an ungrouped `data.frame`.
- The join keys of [`inner_join()`](../reference/mutate_joins.md),
  [`left_join()`](../reference/mutate_joins.md),
  [`right_join()`](../reference/mutate_joins.md) and
  [`full_join()`](../reference/mutate_joins.md) are now cast to their
  common type, as in `dplyr`. For example, joining an integer key to a
  double key gives a double key, a factor key joined to a character key
  gives a character key, and factor keys keep the levels of both tables.
  Previously, the type of the result depended on the join.
- [`semi_join()`](../reference/filter_joins.md) and
  [`anti_join()`](../reference/filter_joins.md) now support a named
  `by`, e.g. `by = c("a" = "b")`. Previously, they looked for the names
  of `y` in `x` and returned the wrong rows.
- Joins on missing columns now give the `dplyr` error, which names the
  missing columns.

Grouped operations now match `dplyr`:

- Groups whose values paste to the same string (e.g. `"a.b"` and `"c"`
  vs `"a"` and `"b.c"`) are no longer merged; the same applies to the
  keys of [`semi_join()`](../reference/filter_joins.md) and
  [`anti_join()`](../reference/filter_joins.md).
- Rows with missing values in grouping columns are no longer dropped by
  grouped verbs.
- Numeric grouping values are compared exactly.
- `group_by(.drop = FALSE)` now includes every combination of factor
  levels and preserves ordered factors.
- [`group_split()`](../reference/group_split.md) now returns groups in
  the same order as [`group_keys()`](../reference/group_split.md), and
  both include empty groups.
- [`group_indices()`](../reference/group_metadata.md) and
  [`cur_group_id()`](../reference/context.md) now count empty groups.
- [`group_data()`](../reference/group_metadata.md) is now correct after
  a grouped [`mutate()`](../reference/mutate.md),
  [`transmute()`](../reference/mutate.md) or
  [`fill()`](../reference/fill.md).
- [`filter()`](../reference/filter.md),
  [`arrange()`](../reference/arrange.md) and
  [`summarise()`](../reference/summarise.md) now respect zero-length
  groups, and `summarise(.groups = "drop")` returns an ungrouped
  `data.frame`.
- Grouped verbs now accept new columns named `f`, `.fn`, `.drop` or
  `.reorder`.
- Grouped verbs now work with zero-row data, and
  [`filter()`](../reference/filter.md) now works with single column
  data.
- [`cur_group_id()`](../reference/context.md) now returns `1` for
  ungrouped data.
- [`summarise()`](../reference/summarise.md) now ignores `NULL` results,
  as in `dplyr` 1.0.4, rather than failing.
- [`summarise()`](../reference/summarise.md) without any columns on
  ungrouped data now returns one row, as in `dplyr`.
- [`tally()`](../reference/count.md) and
  [`count()`](../reference/count.md) no longer report the grouping of
  their summary. [`tally()`](../reference/count.md) now drops only the
  last grouping variable, and [`count()`](../reference/count.md) now
  keeps every grouping variable of its input and its `.drop` setting
  rather than only the first grouping variable.
- [`filter()`](../reference/filter.md) now combines the columns of a
  data frame result, e.g. from [`across()`](../reference/across.md),
  with `&` and warns that this is deprecated, as in `dplyr` 1.0.8.
  Previously, it used the data frame as a matrix index and returned rows
  of `NA`. Matrix results must have one column.
- [`filter()`](../reference/filter.md) now accepts a logical column on
  its own, e.g. `filter(df, is_valid)`, and gives an error for results
  which are not logical rather than silently using them.
- [`summarise()`](../reference/summarise.md) now drops the last group
  when there is only one grouping variable, so the result is ungrouped.
  Previously, empty summaries, list results, zero row data and
  `.drop = FALSE` groups kept the grouping.
- [`bind_rows()`](../reference/bind.md) now regroups its result by the
  groups and `.drop` setting of the first input. Previously, the result
  kept the groups of the first input, so the rows of the other inputs
  belonged to no group
  ([\#82](https://github.com/nathaneastwood/poorman/issues/82)).
- [`lst()`](../reference/lst.md) now evaluates its elements in the
  calling environment, so local variables are found, for example inside
  a function or in the pkgdown examples. The first element can now also
  refer to a variable of the same name, e.g. `lst(x = x + 1)`, rather
  than seeing `NULL`
  ([\#120](https://github.com/nathaneastwood/poorman/issues/120)).

[`summarise()`](../reference/summarise.md) now matches `dplyr`:

- [`summarise()`](../reference/summarise.md) now evaluates expressions
  in the calling environment, so local variables are found, for example
  inside a function. Previously, variables such as `i`, `fns` or `group`
  were silently taken from [`summarise()`](../reference/summarise.md)
  itself and poorman’s internal objects could mask the user’s. As in
  `dplyr` and [`mutate()`](../reference/mutate.md),
  [`n()`](../reference/context.md) and the other context functions must
  now be visible from the calling environment.
- A result which overwrites a column,
  e.g. `summarise(df, x = mean(x), y = x + 1)`, is now used by later
  expressions, and the columns of an unnamed data frame result are now
  available to later expressions.
- Named vector results, e.g. `q = quantile(x, 0.5)`, are now named `q`
  rather than `x`. Unnamed expressions among named ones are now named
  after the expression rather than `""`, long expressions are
  abbreviated as in `dplyr`, e.g. `mean(...)` or `... %>% select(a, b)`
  (unlike `dplyr` 1.0.10, the limit of 60 characters is fixed and counts
  characters rather than bytes, and `?` and `:=` are abbreviated as
  calls; see [`?summarise`](../reference/summarise.md)), and unnamed
  list results are named after the expression.
- Duplicated names now keep the last result rather than giving
  duplicated columns, and a result named as a grouping column now
  replaces it.
- Groups with zero length results now give no rows rather than an error,
  and length one results are recycled to the size of the group. Results
  of incompatible sizes and `NULL` results in only some groups now give
  an informative error.
- Named data frame results, including named
  [`across()`](../reference/across.md) results, are now kept as data
  frame columns and matrix results as matrix columns, rather than being
  spread over several columns.
- [`summarise()`](../reference/summarise.md) can now be used inside
  another [`summarise()`](../reference/summarise.md), e.g. on
  [`cur_data()`](../reference/context.md).
- [`summarise()`](../reference/summarise.md) on grouped data with no
  groups now drops the last grouping variable, as every group has one
  row.
- [`cur_group()`](../reference/context.md) now gives the keys of empty
  groups rather than `NA`.

Selections and pivots now match `tidyr`:

- Selections which evaluate to column names, such as `names(df)[-1]` or
  a function returning column names, now work in
  [`pivot_longer()`](../reference/pivot_longer.md),
  [`pivot_wider()`](../reference/pivot_wider.md),
  [`rename_with()`](../reference/rename.md),
  [`relocate()`](../reference/relocate.md) and
  [`with_groups()`](../reference/with_groups.md), and selections are
  evaluated in the caller’s environment so local variables are found
  ([\#118](https://github.com/nathaneastwood/poorman/issues/118)).
- [`pivot_longer()`](../reference/pivot_longer.md) now matches `tidyr`:
  - Data with character row names, e.g. `mtcars`, can be pivoted and
    rows are kept in their input order.
  - Zero row data, tibbles and grouped data are supported; the class and
    groups of the input are kept.
  - `names_to` supports `".value"` and `NA`, `names_prefix` works with
    several `names_to` and a single `names_to` uses the matching group
    of `names_pattern`.
  - The pivoted columns are combined following the `tidyr` type rules,
    so dates and factors are kept and incompatible types are an error
    rather than being silently coerced.
  - Attributes of the id columns are kept.
  - Invalid combinations of `names_to`, `names_sep`, `names_pattern` and
    `values_to` are now errors.
- [`pivot_longer()`](../reference/pivot_longer.md) gains the `tidyr` 1.1
  arguments `names_ptypes`, `names_transform`, `names_repair`,
  `values_ptypes` and `values_transform`, supports
  `names_to = character()` and keeps every value when the new columns do
  not uniquely identify each pivoted column, e.g. with duplicated column
  names or `NA` in `names_to`.
- [`pivot_wider()`](../reference/pivot_wider.md) now matches `tidyr`:
  - `id_cols` accepts any selection, columns which are not selected are
    dropped and the id columns are returned in the order selected.
  - Factor, date and other classed values keep their class and
    attributes.
  - `values_fill` only fills combinations which are missing from the
    data rather than all `NA`s, works with logical columns and with
    `names_prefix`, keeps integer columns as integer and can be a named
    list.
  - Values which are not uniquely identified now produce list-columns
    and a warning rather than silently keeping the first value.
  - With several `values_from` columns, `names_prefix` is added after
    the value name, e.g. `value_prefix_name`.
  - `names_glue` now works with a single `names_from` column and can use
    `.value`.
  - Zero row data and tibbles are supported; the class and groups of the
    input are kept.
- [`pivot_wider()`](../reference/pivot_wider.md) gains the `tidyr` 1.1
  arguments `names_sort`, `names_repair` and `values_fn`. The warning
  for values which are not uniquely identified now matches `tidyr` 1.1,
  `values_fill` can be `NA` and a `values_fill` which would lose
  precision is an error.

[`arrange()`](../reference/arrange.md) now matches `dplyr`:

- [`arrange()`](../reference/arrange.md) now evaluates each expression
  as its own sort key. Previously, it matched expressions to the columns
  that they used by position, so `arrange(df, x + y, desc(a))` failed
  when `a` was a character column, and expressions without a column,
  such as `arrange(df, n():1)`, failed.
- [`arrange()`](../reference/arrange.md) now only treats a top level
  [`desc()`](../reference/desc.md) as a sort direction. Previously, it
  rewrote [`desc()`](../reference/desc.md) as text, so
  `arrange(df, desc(x + y))` silently sorted by `-x + y`, and
  expressions such as `desc(x) + 0` and long expressions failed.
- [`arrange()`](../reference/arrange.md) now finds variables in the
  calling environment, for example inside a function, and supports the
  `.data` pronoun there.
- [`arrange()`](../reference/arrange.md) now supports
  [`desc()`](../reference/desc.md) on `Date` and other classed columns
  which do not support unary minus.

## poorman 0.2.8

CRAN release: 2026-07-30

Twelfth CRAN release. This release:

- Fixes [`group_split()`](../reference/group_split.md) and
  [`nest_by()`](../reference/nest_by.md) for data with a single column
  ([\#124](https://github.com/nathaneastwood/poorman/issues/124)).
- Uses `names` rather than the deprecated `.Names` in
  [`structure()`](https://rdrr.io/r/base/structure.html).

## poorman 0.2.7

CRAN release: 2023-10-30

Eleventh CRAN release. This release:

- Adds a new function [`fill()`](../reference/fill.md) that mimics
  `tidyr::fill()`
  ([\#113](https://github.com/nathaneastwood/poorman/issues/113),
  [@etiennebacher](https://github.com/etiennebacher)).
- Includes various performance improvements for grouped operations,
  [`pivot_wider()`](../reference/pivot_wider.md) and
  [`pivot_longer()`](../reference/pivot_longer.md)
  ([@etiennebacher](https://github.com/etiennebacher)).
- Fixes CRAN issues.

## poorman 0.2.6

CRAN release: 2022-08-17

Tenth CRAN release.

This update adds the following features:

- Initial versions of [`pivot_longer()`](../reference/pivot_longer.md)
  and [`pivot_wider()`](../reference/pivot_wider.md) have been added
  ([\#101](https://github.com/nathaneastwood/poorman/issues/101),
  [@etiennebacher](https://github.com/etiennebacher)).
- The `.names` argument in [`across()`](../reference/across.md) now
  accepts `{.col}` and `{.fn}` to automatically name new columns
  ([\#100](https://github.com/nathaneastwood/poorman/issues/100),
  [@etiennebacher](https://github.com/etiennebacher)).
- New function [`lst()`](../reference/lst.md) to reproduce the outputs
  of [`tibble::lst()`](https://tibble.tidyverse.org/reference/lst.html).
  It enables the creation of lists where elements are build sequentially
  (i.e depend on the previous ones) and are self-named (i.e the
  expression is used if no name is given)
  ([\#108](https://github.com/nathaneastwood/poorman/issues/108)).

The following fixes were also implemented:

- [`arrange()`](../reference/arrange.md) now works for descending
  character vectors
  ([\#99](https://github.com/nathaneastwood/poorman/issues/99),
  [@etiennebacher](https://github.com/etiennebacher)).
- [`arrange()`](../reference/arrange.md) now resets row names when they
  are numeric
  ([\#102](https://github.com/nathaneastwood/poorman/issues/102),
  [@etiennebacher](https://github.com/etiennebacher)).
- [`distinct()`](../reference/distinct.md) now resets row names when
  they are numeric
  ([\#106](https://github.com/nathaneastwood/poorman/issues/106),
  [@etiennebacher](https://github.com/etiennebacher)).

The CICD pipeline was also fixed such that users can now tests pull
requests. The documentation is now built using HTML5.

## poorman 0.2.5

CRAN release: 2021-03-28

Ninth CRAN release.

This update has added the following new features:

- [`summarise()`](../reference/summarise.md) can now revise grouping
  structures with the `.groups` argument
  ([\#57](https://github.com/nathaneastwood/poorman/issues/57)).
- [`group_by()`](../reference/group_by.md) can now drop groups formed by
  factor levels that don’t appear in the data with the `.drop` parameter
  ([\#63](https://github.com/nathaneastwood/poorman/issues/63)).
- New cumulative functions have been added:
  [`cumall()`](../reference/cummean.md),
  [`cumany()`](../reference/cummean.md) and
  [`cummean()`](../reference/cummean.md)
  ([\#64](https://github.com/nathaneastwood/poorman/issues/64)).
- Additional tools for working with rownames have been added:
  [`has_rownames()`](../reference/rownames.md),
  [`remove_rownames()`](../reference/rownames.md),
  [`column_to_rownames()`](../reference/rownames.md),
  [`rowid_to_column()`](../reference/rownames.md)
  ([\#66](https://github.com/nathaneastwood/poorman/issues/66)).
- [`filter()`](../reference/filter.md) gains the `.preserve` parameters
  ([\#81](https://github.com/nathaneastwood/poorman/issues/81)).
- [`if_any()`](../reference/across.md) and
  [`if_all()`](../reference/across.md) were added
  ([\#80](https://github.com/nathaneastwood/poorman/issues/80)).

The following fixes were also implemented:

- [`mutate()`](../reference/mutate.md) now successfully evaluates
  functions defined in the scope of a parent function
  ([\#68](https://github.com/nathaneastwood/poorman/issues/68)).
- `select_positions()` now works if passed a combination of positive and
  negative values
  ([\#71](https://github.com/nathaneastwood/poorman/issues/71)).
- [`transmute()`](../reference/mutate.md) now keeps unnamed columns that
  it creates
  ([\#72](https://github.com/nathaneastwood/poorman/issues/72)).

The following refactorisations took place:

- When grouping data with [`group_by()`](../reference/group_by.md), it
  will now create an object of class `grouped_df` and not `grouped_data`
  which keeps it consistent with the {dplyr} method name
  ([\#70](https://github.com/nathaneastwood/poorman/issues/70)). This
  should hopefully make transitioning packages from {dplyr} to {poorman}
  a little easier.

## poorman 0.2.4

CRAN release: 2020-11-27

Eighth CRAN release.

This update has added the following new features:

- [`group_by()`](../reference/group_by.md) can now create new columns,
  grouping by the result
  ([\#58](https://github.com/nathaneastwood/poorman/issues/58)).
- [`with_groups()`](../reference/with_groups.md)
  ([\#61](https://github.com/nathaneastwood/poorman/issues/61)).
- [`nth()`](../reference/nth.md)
  ([\#62](https://github.com/nathaneastwood/poorman/issues/62)).
- [`mutate()`](../reference/mutate.md) can now relocate resulting
  mutations
  ([\#59](https://github.com/nathaneastwood/poorman/issues/59)).
- [`mutate()`](../reference/mutate.md) can now keep all, used, unused or
  none of the resulting mutations
  ([\#60](https://github.com/nathaneastwood/poorman/issues/60)).
- [`across()`](../reference/across.md) can now accept lambda style
  functions
  ([\#52](https://github.com/nathaneastwood/poorman/issues/52)).
- Columns created by [`summarise()`](../reference/summarise.md) are now
  immediately available
  ([\#20](https://github.com/nathaneastwood/poorman/issues/20)).
- [`cur_column()`](../reference/context.md)
  ([\#52](https://github.com/nathaneastwood/poorman/issues/52)).

The following fixes were also implemented:

- [`across()`](../reference/across.md) can now accept function names as
  characters
  ([\#56](https://github.com/nathaneastwood/poorman/issues/56)) - thanks
  [@Eyayaw](https://github.com/Eyayaw) for pointing this out.
- An empty [`summarise()`](../reference/summarise.md) will now correctly
  return an empty `data.frame`
  ([\#55](https://github.com/nathaneastwood/poorman/issues/55)).

Other general refactorisations and improvements of note:

- [`group_by()`](../reference/group_by.md) is now an S3 generic.
- `.default()` methods are now `.data.frame()` methods.
- `get_groups()` has been removed in favour of
  [`group_vars()`](../reference/group_metadata.md).
- `set_groups()` has been replaced with `groups_set()` for API
  consistency.

## poorman 0.2.3

CRAN release: 2020-11-12

Seventh CRAN release.

This update has added the following new features:

- [`across()`](../reference/across.md)
  ([\#37](https://github.com/nathaneastwood/poorman/issues/37)).
- `na_matches` and `keep` parameters have been added to mutate joins.
- list columns can now be mutated.
- Added the missing ability to use `&` in `poor-select`.

Fixes:

- Use of variables in [`relocate()`](../reference/relocate.md).

In addition there have been lots of improvements to documentation and
many, many more tests added.

## poorman 0.2.2

CRAN release: 2020-10-11

Sixth CRAN release.

This update has added the following new features:

- [`rename_with()`](../reference/rename.md)
- [`group_cols()`](../reference/group_cols.md)
  ([\#42](https://github.com/nathaneastwood/poorman/issues/42))
- [`cur_data_all()`](../reference/context.md)
- [`nest_by()`](../reference/nest_by.md)
  ([\#44](https://github.com/nathaneastwood/poorman/issues/44))
- [`case_when()`](../reference/case_when.md)
  ([\#23](https://github.com/nathaneastwood/poorman/issues/23))
- [`bind_cols()`](../reference/bind.md)
  ([\#32](https://github.com/nathaneastwood/poorman/issues/32))
- [`bind_rows()`](../reference/bind.md)
  ([\#33](https://github.com/nathaneastwood/poorman/issues/33))
- [`unite()`](../reference/unite.md)

Performance upgrades and code improvements:

- [`pull()`](../reference/pull.md) is now much faster thanks to
  [@markfairbanks](https://github.com/markfairbanks).
- `%>%` now allows you to pipe into a `.`; my thanks go to
  [@moodymudskipper](https://github.com/moodymudskipper).

The following bugs have been squashed:

- Filter joins will no longer reduce to vectors thanks to
  [@msberends](https://github.com/msberends).
- [`contains()`](../reference/select_helpers.md) no longer returns
  lists.
- [`relocate()`](../reference/relocate.md) can now use “tidy select”
  style column selections.

New documentation components have been added:

- An FAQ vignette has been added
  ([\#31](https://github.com/nathaneastwood/poorman/issues/31)).
- A [pkgdown](https://pkgdown.r-lib.org/) website has been added
  ([\#30](https://github.com/nathaneastwood/poorman/issues/30)).

## poorman 0.2.1

CRAN release: 2020-07-01

Fifth CRAN release.

The update has added the following new features:

- [`distinct()`](../reference/distinct.md)
  ([\#17](https://github.com/nathaneastwood/poorman/issues/17))
- [`slice_head()`](../reference/slice.md),
  [`slice_tail()`](../reference/slice.md),
  [`slice_min()`](../reference/slice.md),
  [`slice_max()`](../reference/slice.md),
  [`slice_sample()`](../reference/slice.md)
  ([\#22](https://github.com/nathaneastwood/poorman/issues/22))
- [`where()`](../reference/where.md)
  ([\#27](https://github.com/nathaneastwood/poorman/issues/27))
- [`coalesce()`](../reference/coalesce.md)
  ([\#28](https://github.com/nathaneastwood/poorman/issues/28))
- [`group_split()`](../reference/group_split.md),
  [`group_keys()`](../reference/group_split.md)
  ([\#29](https://github.com/nathaneastwood/poorman/issues/29))
- [`na_if()`](../reference/na_if.md)
  ([\#34](https://github.com/nathaneastwood/poorman/issues/34))
- [`recode()`](../reference/recode.md)
  ([\#35](https://github.com/nathaneastwood/poorman/issues/35))
- [`near()`](../reference/near.md)
  ([\#36](https://github.com/nathaneastwood/poorman/issues/36))
- [`replace_na()`](../reference/replace_na.md)
  ([\#38](https://github.com/nathaneastwood/poorman/issues/38))
- [`group_data()`](../reference/group_metadata.md),
  [`group_indices()`](../reference/group_metadata.md),
  [`group_vars()`](../reference/group_metadata.md),
  [`group_rows()`](../reference/group_metadata.md),
  [`group_size()`](../reference/group_metadata.md),
  [`n_groups()`](../reference/group_metadata.md),
  [`groups()`](../reference/group_metadata.md)
  ([\#39](https://github.com/nathaneastwood/poorman/issues/39))
- [`glimpse()`](../reference/glimpse.md)
  ([\#40](https://github.com/nathaneastwood/poorman/issues/40))
- [`cur_data()`](../reference/context.md),
  [`cur_group()`](../reference/context.md),
  [`cur_group_id()`](../reference/context.md),
  [`cur_group_rows()`](../reference/context.md)
  ([\#41](https://github.com/nathaneastwood/poorman/issues/41))

There have also been a lot of refactoring for the internals of the
package, particularly focusing on context awareness which allows for
quite a few of these features
([\#40](https://github.com/nathaneastwood/poorman/issues/40),
[\#41](https://github.com/nathaneastwood/poorman/issues/41)) and grouped
operations should now be more consistent. This update of course comes
with a host of bug fixes including the renaming of multiple columns when
using [poorman](https://nathaneastwood.github.io/poorman/)’s version of
[tidyselect](https://tidyselect.r-lib.org).

## poorman 0.2.0

CRAN release: 2020-05-10

Fourth CRAN release.

This update has added the following new features:

- [`if_else()`](../reference/if_else.md)
  ([\#11](https://github.com/nathaneastwood/poorman/issues/11))
- [`between()`](../reference/between.md)
  ([\#12](https://github.com/nathaneastwood/poorman/issues/12))
- [`lead()`](../reference/lag.md) and [`lag()`](../reference/lag.md)
  ([\#13](https://github.com/nathaneastwood/poorman/issues/13))
- [`n_distinct()`](../reference/n_distinct.md)
  ([\#16](https://github.com/nathaneastwood/poorman/issues/16))
- window rank functions: [`cume_dist()`](../reference/window_rank.md),
  [`dense_rank()`](../reference/window_rank.md),
  [`min_rank()`](../reference/window_rank.md),
  [`ntile()`](../reference/window_rank.md),
  [`percent_rank()`](../reference/window_rank.md),
  [`row_number()`](../reference/window_rank.md)
  ([\#18](https://github.com/nathaneastwood/poorman/issues/18))

My thanks go to [@msberends](https://github.com/msberends) for their
help on these features!

This update also refactors a lot of the code, hence the minor version
bump. This includes:

- More support for “tidyselect” features such as `!`, `:`, `-`,
  [`c()`](https://rdrr.io/r/base/c.html) and `()`
- Removal of `eval(parse(text = ...)`
- Rework of grouped summaries

Finally, 0.2.0 includes many bug fixes:

- Joins now maintain their original order
  ([\#10](https://github.com/nathaneastwood/poorman/issues/10))
- [`group_by()`](../reference/group_by.md) +
  [`mutate()`](../reference/mutate.md) now preserve row order
  ([\#14](https://github.com/nathaneastwood/poorman/issues/14))
- Quote names are allowed in [`pull()`](../reference/pull.md)
  ([\#15](https://github.com/nathaneastwood/poorman/issues/15))

## poorman 0.1.11

CRAN release: 2020-04-22

Third CRAN release.

This update adds the functions [`count()`](../reference/count.md),
[`tally()`](../reference/count.md),
[`add_count()`](../reference/count.md) and
[`add_tally()`](../reference/count.md). In order to achieve these, the
functions [`n()`](../reference/context.md) and
[`desc()`](../reference/desc.md) have also been added. In addition,
[`group_by()`](../reference/group_by.md) can now add additional groups
to an already grouped `data.frame`.

## poorman 0.1.10

CRAN release: 2020-04-13

Second CRAN release.

This update adds selection helper functions. See
[`?select_helpers`](../reference/select_helpers.md) for more
information.

The following bugs have been fixed:

- [`mutate()`](../reference/mutate.md) column creations are immediately
  available, e.g. `mtcars %>% mutate(mpg2 = mpg * 2, mpg4 = mpg2 * 2)`
  will create columns named `mpg2` and `mpg4`
- [`group_by()`](../reference/group_by.md) groups now persist in
  selections, e.g. `mtcars %>% group_by(am) %>% select(mpg)` will return
  `am` and `mpg` columns
- [`slice()`](../reference/slice.md) now duplicates rows,
  e.g. `mtcars %>% slice(2, 2, 2)` will return row 2 three times
- [`summarize()`](../reference/summarise.md) is now exported

## poorman 0.1.9

CRAN release: 2020-04-01

First CRAN release

## poorman 0.1.8

This update adds [`relocate()`](../reference/relocate.md) and
consolidates the selection method for
[`select()`](../reference/select.md),
[`rename()`](../reference/rename.md) and
[`relocate()`](../reference/relocate.md) using `select_positions()`.

## poorman 0.1.7

This update adds [`semi_join()`](../reference/filter_joins.md) and
[`anti_join()`](../reference/filter_joins.md).

## poorman 0.1.6

This update adds [`inner_join()`](../reference/mutate_joins.md),
[`left_join()`](../reference/mutate_joins.md),
[`right_join()`](../reference/mutate_joins.md) and
[`full_join()`](../reference/mutate_joins.md).

## poorman 0.1.5

This update adds a full set of tests for the functions available in
v0.1.5.

## poorman 0.1.4

This update adds a copy of the pipe (`%>%`).

## poorman 0.1.3

This update adds [`summarise()`](../reference/summarise.md).

## poorman 0.1.2

This update adds [`rename()`](../reference/rename.md).

## poorman 0.1.1

This update includes [`group_by()`](../reference/group_by.md) and
[`ungroup()`](../reference/group_by.md) operations.

## poorman 0.1.0

This initial version includes the [`select()`](../reference/select.md),
[`pull()`](../reference/pull.md),
[`arrange()`](../reference/arrange.md),
[`filter()`](../reference/filter.md),
[`slice()`](../reference/slice.md), [`mutate()`](../reference/mutate.md)
and [`transmute()`](../reference/mutate.md) functions. See the [blog
post](https://nathaneastwood.github.io/2020/02/15/building-a-base-dplyr-with-primitives/)
for more details.
