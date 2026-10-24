# Package index

## One table verbs

- [`arrange()`](arrange.md) : Arrange rows by variables
- [`count()`](count.md) [`tally()`](count.md) [`add_count()`](count.md)
  [`add_tally()`](count.md) : Count observations by group
- [`distinct()`](distinct.md) : Subset distinct/unique rows
- [`fill()`](fill.md) : Fill in missing values with previous or next
  value
- [`filter()`](filter.md) : Return rows with matching conditions
- [`mutate()`](mutate.md) [`transmute()`](mutate.md) : Create or
  transform variables
- [`pull()`](pull.md) : Pull out a single variable
- [`relocate()`](relocate.md) : Change column order
- [`rename()`](rename.md) [`rename_with()`](rename.md) : Rename columns
- [`select()`](select.md) : Subset columns using their names and types
- [`summarise()`](summarise.md) [`summarize()`](summarise.md) : Reduce
  multiple values down to a single value
- [`slice()`](slice.md) [`slice_head()`](slice.md)
  [`slice_tail()`](slice.md) [`slice_min()`](slice.md)
  [`slice_max()`](slice.md) [`slice_sample()`](slice.md) : Subset rows
  by position
- [`unite()`](unite.md) : Unite Multiple Columns Into One

## Two table verbs

- [`bind_cols()`](bind.md) [`bind_rows()`](bind.md) :

  Efficiently bind multiple `data.frame`s by row and column

- [`anti_join()`](filter_joins.md) [`semi_join()`](filter_joins.md) :

  Filtering joins filter rows from `x` based on the presence or absence
  of matches in `y`:

- [`inner_join()`](mutate_joins.md) [`left_join()`](mutate_joins.md)
  [`right_join()`](mutate_joins.md) [`full_join()`](mutate_joins.md) :
  Mutating Joins

- [`nest_join()`](nest_join.md) : Nest Join

- [`union_all()`](union_all.md) : Union All

## Pivoting

- [`pivot_longer()`](pivot_longer.md) : Pivot data from wide to long
- [`pivot_wider()`](pivot_wider.md) : Pivot data from long to wide

## Grouping

- [`group_by()`](group_by.md) [`ungroup()`](group_by.md) : Group by one
  or more variables
- [`group_by_drop_default()`](group_by_drop_default.md) : Default value
  for .drop argument of group_by
- [`group_data()`](group_metadata.md)
  [`group_rows()`](group_metadata.md)
  [`group_indices()`](group_metadata.md)
  [`group_vars()`](group_metadata.md) [`groups()`](group_metadata.md)
  [`group_size()`](group_metadata.md) [`n_groups()`](group_metadata.md)
  : Grouping metadata
- [`group_split()`](group_split.md) [`group_keys()`](group_split.md) :
  Split data.frame by groups
- [`n()`](context.md) [`cur_data()`](context.md)
  [`cur_data_all()`](context.md) [`cur_group()`](context.md)
  [`cur_group_id()`](context.md) [`cur_group_rows()`](context.md)
  [`cur_column()`](context.md) : Context dependent expressions
- [`nest_by()`](nest_by.md) : Nest By
- [`with_groups()`](with_groups.md) : Perform an operation with
  temporary groups

## Vector functions

- [`across()`](across.md) [`if_any()`](across.md)
  [`if_all()`](across.md) : Apply a function (or functions) across
  multiple columns

- [`between()`](between.md) : Do values in a numeric vector fall in
  specified range?

- [`case_when()`](case_when.md) :

  A General Vetorised `if()`

- [`coalesce()`](coalesce.md) : Find first non-missing element

- [`cummean()`](cummean.md) [`cumany()`](cummean.md)
  [`cumall()`](cummean.md) : Cumulative versions of any, all, and mean

- [`desc()`](desc.md) : Descending order

- [`if_else()`](if_else.md) : Vectorised if

- [`lag()`](lag.md) [`lead()`](lag.md) : Compute lagged or leading
  values

- [`n_distinct()`](n_distinct.md) : Count the number of unique values in
  a set of vectors

- [`na_if()`](na_if.md) : Convert values to NA

- [`near()`](near.md) : Compare two numeric vectors

- [`nth()`](nth.md) [`first()`](nth.md) [`last()`](nth.md) : Extract the
  first, last or nth value from a vector

- [`recode()`](recode.md) [`recode_factor()`](recode.md) : Recode values

- [`replace_na()`](replace_na.md) : Replace missing values

- [`cume_dist()`](window_rank.md) [`dense_rank()`](window_rank.md)
  [`min_rank()`](window_rank.md) [`ntile()`](window_rank.md)
  [`percent_rank()`](window_rank.md) [`row_number()`](window_rank.md) :
  Windowed Rank Functions

## Selection helpers

- [`group_cols()`](group_cols.md) : Select Grouping Variables
- [`peek_vars()`](peek_vars.md) : Peek at variables in the selection
  context
- [`starts_with()`](select_helpers.md)
  [`ends_with()`](select_helpers.md) [`contains()`](select_helpers.md)
  [`matches()`](select_helpers.md) [`num_range()`](select_helpers.md)
  [`all_of()`](select_helpers.md) [`any_of()`](select_helpers.md)
  [`everything()`](select_helpers.md) [`last_col()`](select_helpers.md)
  : Select Helpers
- [`where()`](where.md) : Select variables with a function

## Piping

- [`` `%>%` ``](pipe.md) : Forward-pipe operator

## data.frame Display

- [`glimpse()`](glimpse.md) : Get a glimpse of your data

## data.frame Attributes

- [`rownames_to_column()`](rownames.md)
  [`rowid_to_column()`](rownames.md)
  [`column_to_rownames()`](rownames.md)
  [`remove_rownames()`](rownames.md) [`has_rownames()`](rownames.md) :
  Tools for working with row names

## Utilities

- [`lst()`](lst.md) : Build a list
