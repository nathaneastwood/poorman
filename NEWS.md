# poorman 0.3.1

## Improvements

`across()`, `if_any()` and `if_all()` are now considerably faster on grouped data. The columns, the functions and the names of the results are found once for each expression rather than once for each group, the columns are read from the data of the group rather than from a copy of it, and the data frame results of the groups are bound column by column rather than with `rbind()`. On 100,000 rows, `summarise()` with `across()` is 2.4-4x faster with 1,000 and 10,000 groups, `mutate()` with `across()` is 3-3.5x faster from 10 groups and allocates 2-5x less memory, and `filter()` with `if_any()` is 2.1-2.9x faster. A lambda, e.g. `~ .x - mean(.x)`, is converted to a function once for each expression rather than once for each group, which makes `mutate()` and `summarise()` with lambdas in `across()` a further 1.13-1.23x faster with 1,000 and 10,000 groups.

## Bug fixes

* `across()`, `if_any()` and `if_all()` now select their columns once for each expression rather than once for each group, and `where()` applies its predicate to the full columns rather than to the rows of each group, as in `dplyr`. A column made by an earlier expression is tested in full, e.g. in the order of the rows in `mutate()`. Previously, groups could select different columns, which gave the error "names do not match previous names".
* The column selection of `across()`, `if_any()` and `if_all()` now finds objects in the environment where it is called, e.g. `across(all_of(cols), sum)` or `across(where(is_big), max)` inside a function where `cols` or `is_big` is defined, as in `dplyr`. Previously, it gave the error "object 'cols' not found".
* `across()`, `if_any()` and `if_all()` now select from the columns other than the grouping columns, as in `dplyr`. Positions count the other columns, e.g. `across(1)` selects the first column which is not a grouping column, selecting a grouping column, e.g. `across(g)`, gives an error, and `where()` no longer applies its predicate to the grouping columns. Previously, the selection included the grouping columns and then dropped them, so `across(1)` on data grouped by its first column selected nothing. Unlike `dplyr` 1.0.10, where `group_cols()` gives an error within `across()`, it selects no columns, so `across(-group_cols())` selects every column (see `?across`).
* `where()` now accepts a formula, e.g. `where(~ is.numeric(.x) && mean(.x) > 3)`, as in `dplyr`. Previously, it gave the error "is not a valid predicate function".
* A selection which starts with a negation now starts from every column, even when the negation matches no column, as in `dplyr`. For example, `select(df, -group_cols())` on data which is not grouped and `select(df, -starts_with("z"))` with no column starting with "z" now select every column. Previously, they selected none.
* `summarise()` now ignores the size of an unnamed data frame with no columns, such as the result of an `across()` which selects no columns, as in `dplyr`. Previously, grouped `summarise(df, across(starts_with("z"), sum))` gave a row for each row of the data rather than for each group.

# poorman 0.3.0

## Breaking changes

* The arguments of `pivot_wider()` are now in the same order as in `tidyr` 1.1: `data`, `id_cols`, `names_from`, `names_prefix`, `names_sep`, `names_glue`, `names_sort`, `names_repair`, `values_from`, `values_fill`, `values_fn`. Code which passes `values_from` or `names_from` by position must now name them.
* Joins now give an error when the join keys of `x` and `y` have incompatible types, such as integer and character, as in `dplyr`. Previously, the keys were silently converted. A natural join of tables with no common columns is now also an error; previously, the mutating joins returned a cross join and the filtering joins returned or dropped every row. Use `by = character()` for a cross join.
* The mutating joins no longer pass `...` to `merge()`; as in `dplyr`, arguments in `...` are ignored.
* `inner_join()`, `left_join()`, `right_join()` and `full_join()` gain a `copy` argument, which is ignored, before `suffix`, as in `dplyr`. Code which passes `suffix` by position must now name it.
* `count()`, `tally()`, `add_count()` and `add_tally()` no longer use a column named `n` as the weight when `wt` is omitted, and no longer give the message "Using `n` as weighting variable". As in `dplyr`, they count the rows; use `wt = n` to weight by the column. A string given as `wt`, e.g. `wt = "x"`, is no longer taken as a column name; as in `dplyr`, it gives an error.

## New features

* `arrange()` now supports `across()` (#88).
* `arrange()` now supports list, data frame and matrix columns. Data frame and matrix columns are sorted by each of their columns in turn and list columns are sorted by the first appearance of each element, as in `dplyr` (#90).
* New `nest_join()` returns all rows and columns of `x` with a list column of the matching rows of `y`, as `data.frame`s. Rows of `x` with no match get a 0-row `data.frame` (#78).
* Arguments can now be named with `:=`, as in `dplyr`, e.g. `summarise(df, total := sum(x))`. A string on the left is a `gluestick` template, which is filled in from the calling environment, e.g. `mutate(df, "{var}_mean" := mean(x))`. This works in every verb whose arguments take names, such as `summarise()`, `mutate()`, `transmute()`, `rename()`, `select()` and `distinct()`. Unlike `dplyr`, `!!` and `{{` are not supported on the left of `:=`.
* `semi_join()` and `anti_join()` gain the `copy`, `...` and `na_matches` arguments of `dplyr`. `copy` and `...` are ignored. Use `na_matches = "never"` to stop keys with `NA` or `NaN` values from matching.

## Improvements

`inner_join()`, `left_join()`, `right_join()` and `full_join()` have been rewritten without `merge()`. Rows are matched by sorting `y` by key once rather than by `merge()`, and the result is built from the matched row numbers. On 100,000 rows, the joins are 13-16x faster and allocate 2.5x less memory; on 1,000,000 rows, they are 13-20x faster.

Grouped operations are now considerably faster. Groups are identified in a single pass over the data rather than by comparing every group against every row, so the cost of `group_by()` no longer grows with the number of groups. On 100,000 rows with 1,000 groups, `group_by()` and `arrange()` are over 100x faster, `filter()`, `mutate()` and `count()` 7-20x faster, `group_indices()` over 1,000x faster, and grouping by several high cardinality columns no longer exhausts memory.

`pivot_longer()` and `pivot_wider()` have been rewritten without `stats::reshape()` and are now considerably faster and use far less memory. On 100,000 rows by 10 columns, `pivot_longer()` is over 35x faster; on 1,000,000 rows, `pivot_wider()` is over 15x faster, and widening to 1,000 columns is over 150x faster and no longer allocates 3.5GB. Both functions now follow `tidyr` 1.1.

`summarise()` is now considerably faster on grouped data. Each column is split by group once and the summaries are combined column by column, rather than building and binding a `data.frame` for each group. On 100,000 rows, `summarise()` is over 4x faster with 1,000 groups and 5x faster with 10,000 groups, and `count()` with 26,000 groups is over 7x faster. `across()` builds its result faster, which also speeds up `mutate()`.

`mutate()` and `transmute()` are now considerably faster on grouped data. As in `summarise()`, each column is split by group once and the expressions are evaluated for each group in a data mask, rather than building and binding a `data.frame` for each group. On 100,000 rows, grouped `mutate()` is 17x faster with 10 groups, 10x faster with 1,000 groups and 6x faster with 10,000 groups, and allocates 3x less memory; `transmute()` is 10-20x faster.

`filter()` is now considerably faster. As in `mutate()`, the conditions are evaluated for each group in a data mask and the rows are subset once, rather than building and binding a `data.frame` for each group. On 100,000 rows, grouped `filter()` is 7-10x faster with 10 groups, 5x faster with 1,000 groups and 3-3.6x faster with 10,000 groups, and allocates about half the memory; on 1,000,000 rows with 10,000 groups, it is 5-6x faster. Ungrouped `filter()` is 2-3x faster.

`slice()`, `slice_head()`, `slice_tail()`, `slice_min()`, `slice_max()`, `slice_sample()`, `distinct()` and `fill()` are now considerably faster on grouped data. The rows to keep are found for each group and the data is subset once, and `fill()` fills every group in one pass, rather than building and binding a `data.frame` for each group. On 100,000 rows with 1,000 groups, `slice_head()` is 28x faster, `slice()` and `slice_min()` 3-5x faster, `slice_sample()` 14x faster, `distinct()` 20x faster and `fill()` 79x faster; with 10,000 groups, `fill()` is over 400x faster.

## Bug fixes

* `keep = TRUE` now works as in `dplyr`. The keys of `x` and `y` are both kept, each with `NA` where a row has no match in its table; previously, the key of `y` repeated the key of `x`, and `keep = TRUE` with a named `by` failed. `inner_join()` now supports `keep` too.
* The columns of a join are now in the order of `x` followed by `y`, and the rows of `y` without a match now come last in their order in `y`, as in `dplyr`. Previously, the key columns came first and the rows of `y` without a match were sorted by key. Suffixes are now repeated until the names are unique, e.g. `k.x.x`.
* A join which renames all of the grouping columns of `x`, e.g. with `keep = TRUE`, now returns an ungrouped `data.frame`.
* The join keys of `inner_join()`, `left_join()`, `right_join()` and `full_join()` are now cast to their common type, as in `dplyr`. For example, joining an integer key to a double key gives a double key, a factor key joined to a character key gives a character key, and factor keys keep the levels of both tables. Previously, the type of the result depended on the join.
* `semi_join()` and `anti_join()` now support a named `by`, e.g. `by = c("a" = "b")`. Previously, they looked for the names of `y` in `x` and returned the wrong rows.
* Joins on missing columns now give the `dplyr` error, which names the missing columns.
* `select()` now supports negative selections on grouped data, e.g. `select(group_by(df, id), -g)`. Previously, it failed with "only 0's may be mixed with negative subscripts". A deselected grouping variable is added back in front, with a message, as in `dplyr`.
* `rename_with()` no longer renames grouping variables which were not selected. Previously, it added them to the selection with an "Adding missing grouping variables" message.
* `mutate()` and `transmute()` now support the `.data` pronoun, e.g. `mutate(df, z = .data[[var]] * 2)`. As in `dplyr`, `.data` sees the columns made by earlier expressions and, on grouped data, holds the rows of the current group. Previously, `.data` gave the error "object '.data' not found".
* `across()` and the `.data` pronoun in `summarise()` now see the summaries made by earlier expressions, e.g. `summarise(df, m = mean(x), across(m, ~ .x * 10))`, as in `dplyr`. Previously, `across()` gave the error "Column m does not exist" and `.data$m` gave a result with no rows. As in `dplyr`, each expression is now evaluated for every group before the next expression.
* Grouped `mutate()` and `transmute()` now evaluate their expressions for empty groups, such as the unused levels of a factor grouped with `.drop = FALSE`, as in `dplyr`. As in `dplyr`, each expression is now evaluated for every group before the next expression.
* `mutate()` and `transmute()` now give the `dplyr` error when a result has the wrong size, e.g. "x `k` must be size 3 or 1, not 2", or "x `k` must be size 1, not 2" when the data or the group has one row, naming the group on grouped data. Previously, they gave the error of `[[<-.data.frame`, "replacement has 2 rows, data has 3".
* `mutate()`, `transmute()` and `summarise()` now give the `dplyr` error when the results of the groups have incompatible types, e.g. a character in one group and a double in another: "`y` must return compatible vectors across groups", followed by the type of the result of each of two groups, e.g. "i Result type for group 1 (g = 1): <character>.". Previously, `summarise()` silently combined them, e.g. into a character, and `mutate()` gave the error "Can't combine `` <character> and `` <double>". Integers and doubles, factors and characters, and logical `NA`s and any type are still combined. Unlike `dplyr`, a factor is named `<factor>` and a data frame `<data.frame>`, without the levels hash or column types (see `?mutate`).
* Grouped `mutate()`, `transmute()` and `summarise()` now combine a `Date` result in one group and a `POSIXct` result in another into a `POSIXct`, as in `dplyr`, with each date at midnight in the time zone of the result. Previously, the result was a `Date` and the times were lost. Combined datetimes now take the first time zone which is set, or the local time zone, as in `dplyr`; previously, a datetime without a time zone in the first group dropped the time zone of the others, and a logical `NA` before a date and a datetime gave a plain number.
* Grouped `mutate()`, `transmute()` and `summarise()` now give the `dplyr` error when an expression returns `NULL` in only some groups: "Problem while computing `y = ...`. x `y` must return compatible vectors across groups. x Can't combine NULL and non NULL results.". Previously, `mutate()` gave the error "numbers of columns of arguments do not match" and `summarise()` gave a shorter error.
* A verb can now be used inside the expressions of another, e.g. `mutate(group_by(df, g), k = nrow(filter(df, x > 2)) + n())`. Previously, the inner verb removed the data of the outer one, so `n()` and the other context functions failed after it.
* `transmute()` now finds variables in the calling environment, e.g. inside a function. Previously, it gave an "object not found" error, or found a function of the same name.
* `mutate()`, `transmute()` and `group_by()` now give unnamed expressions the same automatic names as `summarise()`. Long expressions are abbreviated, e.g. `... + x`, and `.data$x` is named `x`. Previously, an expression which R deparses over several lines gave a broken name such as `c("{", "    x + 1", "}")`.
* Automatic names now show the value of the subscript of `.data[[...]]`, as in `dplyr`. With `var <- "y"`, `summarise(df, .data[[var]])` is named `y` and `summarise(df, mean(.data[[var]]))` is named `mean(.data[["y"]])`. Previously, they were named `.data[[var]]` and `mean(.data[[var]])`.
* `group_by()` and `count()` now group by renamed columns and literals, e.g. `group_by(df, total = g)` or `count(df, 5)`, and add columns with `.add = TRUE`, e.g. `group_by(df, h = x, .add = TRUE)`. Previously, these gave an "Invalid groups" error. They also now find variables in the calling environment, e.g. inside a function.
* `filter()` now checks the size of each condition and gives the `dplyr` error, e.g. "x Input `..1` must be of size 3 or 1, not size 2", naming the group on grouped data. Previously, a condition of the wrong size was recycled, and a condition of size one on data without rows returned a row of `NA`. The error for a condition which is not logical now names the group too, and a data frame result is checked column by column, e.g. "x Input `..1$a` must be a logical vector, not a integer".
* `filter()` now supports the `.data` pronoun, e.g. `filter(df, .data[[var]] > 2)`. Previously, grouped `filter()` returned rows of `NA` and ungrouped `filter()` gave the error "object '.data' not found".
* `add_count()` now returns the groups of its input, as its documentation says and as `count()` does. Previously, it also grouped the result by the variables it counted, e.g. `add_count(df, g)` returned data grouped by `g`.
* `add_count(sort = TRUE)` now sorts the rows by the count, largest first. Previously, `sort` was ignored.
* `count()`, `tally()`, `add_count()` and `add_tally()` no longer give an error when the input has a column named `n` and `name` is omitted. As in `dplyr`, `count()` and `tally()` store the count in `n` when the column `n` is summarised away, and otherwise, e.g. `add_count(df, g)` or `count(df, n)`, store it in `nn` (or `nnn`, ...) with the message "Storing counts in `nn`, as `n` already present in input".
* `add_count()`, `group_split()` and `nest_by()` now find variables in the calling environment, e.g. inside a function. Previously, they gave an "object not found" error, or silently used a global variable of the same name.
* The `wt` argument of `count()`, `tally()`, `add_count()` and `add_tally()` now takes an expression of columns and finds variables in the calling environment, e.g. `wt = x * k` inside a function, as in `dplyr`. Previously, `wt` had to be a bare column name, and an expression or a local variable gave an "object not found" error. `wt = n()` now gives the `dplyr` deprecation warning and counts the rows.
* A `mutate()` called inside the arguments of `transmute()`, `group_by()` or `count()`, e.g. in a user function, now finds variables in its own calling environment. Previously, it looked in the environment of the outer verb's caller and gave an "object not found" error.
* Grouped `filter()` now finds variables in the calling environment, e.g. inside a function. Previously, it gave an "object not found" error.
* Grouped `filter()` now evaluates its conditions for empty groups, and evaluates every condition for a group before the next group, as in `dplyr`. It now gives the deprecation warning for data frame results once for each condition which returns one, as in `dplyr`. Previously, conditions with the same warning shared a single warning.
* `if_any()` and `if_all()` now return `TRUE` when no columns are selected, as in `dplyr`, so `filter()` keeps every row. Previously, they returned `FALSE`, so `filter()` dropped every row.
* `filter()` now gives the `dplyr` error for a named condition, e.g. `filter(df, x = 1)`: "We detected a named input. i This usually means that you've used `=` instead of `==`. i Did you mean `x == 1`?". Previously, it gave an older wording, and a named expression or variable, e.g. `filter(df, z = x == 1)`, failed with "invalid type of argument[2]: 'language'".
* Grouped `slice()`, `slice_min()`, `slice_max()`, `slice_sample()`, `distinct()` and `fill()` now find variables in the calling environment, e.g. inside a function. Previously, they gave an "object not found" error.
* `slice_min()`, `slice_max()` and `slice_sample()` now evaluate `order_by` and `weight_by` as expressions, e.g. `slice_min(df, x * y)` or `slice_sample(df, n = 1, weight_by = x > 0)`, and check that they have a value for each row, as in `dplyr`. Previously, they only accepted a column name.
* Grouped `distinct()` now returns the rows in their original order, as in `dplyr`. Previously, the rows were in group order. It now also keeps the empty groups of data grouped with `.drop = FALSE`.
* Grouped `slice()` without indices now returns the data unchanged, as in `dplyr`. Previously, the rows were put in group order.
* `slice_tail()` now rounds `n` down, e.g. `slice_tail(df, n = 2.5)` returns the last 2 rows. Previously, it returned the wrong rows.
* `slice_head()`, `slice_tail()`, `slice_min()` and `slice_max()` now accept a negative `n` or `prop`, which gives the number or proportion of rows to drop from each group, as in `dplyr`. For example, `slice_tail(df, n = -1)` returns all rows but the first in each group. Previously, it was an error. `slice_sample()` gives the error "`n` must be positive." for a negative `n`, and the errors for a missing `n` or `prop`, or for both, now have the same text as in `dplyr`.
* `slice_head()`, `slice_tail()`, `slice_min()`, `slice_max()` and `slice_sample()` select no rows for `n = 0` or `prop = 0`, and `slice_min()` and `slice_max()` with `with_ties = FALSE` select no rows from a group when a negative `n` or `prop` drops at least as many rows as it has. This differs from `dplyr` 1.0.10, where `n = 0` and `prop = 0` return every row in `slice_head()`, `slice_tail()`, `slice_min()` and `slice_max()` and are an error in `slice_sample()`, and where `slice_min()` and `slice_max()` with `with_ties = FALSE` return rows again when a negative `n` is larger than the group, e.g. `n = -10` on 7 rows returns 4 rows. Later versions of `dplyr` select no rows in each case (see `?slice`).
* `fill()` now replaces `NULL`s in list columns, as documented. Grouped `fill()` no longer gives an "Adding missing grouping variables" message for each group.
* Each group of `mutate()`, `transmute()`, `summarise()` and `filter()` now has its own variables, so a variable assigned inside an expression for one group is not seen by the next group. In `dplyr` 1.0.10, such a variable leaks into the groups evaluated after it.

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
* `bind_rows()` now regroups its result by the groups and `.drop` setting of the first input. Previously, the result kept the groups of the first input, so the rows of the other inputs belonged to no group (#82).
* `lst()` now evaluates its elements in the calling environment, so local variables are found, for example inside a function or in the pkgdown examples. The first element can now also refer to a variable of the same name, e.g. `lst(x = x + 1)`, rather than seeing `NULL` (#120).

`summarise()` now matches `dplyr`:

* `summarise()` now evaluates expressions in the calling environment, so local variables are found, for example inside a function. Previously, variables such as `i`, `fns` or `group` were silently taken from `summarise()` itself and poorman's internal objects could mask the user's. As in `dplyr` and `mutate()`, `n()` and the other context functions must now be visible from the calling environment.
* A result which overwrites a column, e.g. `summarise(df, x = mean(x), y = x + 1)`, is now used by later expressions, and the columns of an unnamed data frame result are now available to later expressions.
* Named vector results, e.g. `q = quantile(x, 0.5)`, are now named `q` rather than `x`. Unnamed expressions among named ones are now named after the expression rather than `""`, long expressions are abbreviated as in `dplyr`, e.g. `mean(...)` or `... %>% select(a, b)` (unlike `dplyr` 1.0.10, the limit of 60 characters is fixed and counts characters rather than bytes, and `?` and `:=` are abbreviated as calls; see `?summarise`), and unnamed list results are named after the expression.
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

# poorman 0.2.8

Twelfth CRAN release. This release:

* Fixes `group_split()` and `nest_by()` for data with a single column (#124).
* Uses `names` rather than the deprecated `.Names` in `structure()`.

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
