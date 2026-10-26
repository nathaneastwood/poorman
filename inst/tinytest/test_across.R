options(stringsAsFactors = FALSE)

expect_equal(
  data.frame(x = 1) %>% mutate(across()),
  data.frame(x = 1),
  info = "across() works on one column data.frame"
)

expect_equivalent(
  mtcars %>% group_by(cyl) %>% summarise(across(starts_with("c"), mean)) %>% ungroup(),
  data.frame(cyl = c(4, 6, 8), carb_1 = c(1.54545455, 3.42857, 3.5)),
  tolerance = 0.00001,
  info = "across() does not select grouping variables"
)

gf <- data.frame(x = 1, y = 2, z = 3, s = "") %>% group_by(x)
expect_named <- function(x, y, info = NA_character_) expect_equal(colnames(x), y, info = info)
expect_named(
  summarise(gf, across()),
  c("x", "y", "z", "s"),
  "across() correctly names output columns"
)
expect_named(
  summarise(gf, across(where(is.numeric), mean)),
  c("x", "y", "z"),
  "across() correctly names output for non-named functions"
)
expect_named(
  summarise(gf, across(where(is.numeric), list(mean = mean, sum = sum))),
  c("x", "y_mean", "y_sum", "z_mean", "z_sum"),
  info = "across() correctly names output columns for named lists of functions"
)
expect_named(
  summarise(gf, across(where(is.numeric), list(mean = mean, sum = sum),
                       .names = "my_col_{.col}_{.fn}")),
  c("x", "my_col_y_mean", "my_col_y_sum", "my_col_z_mean", "my_col_z_sum"),
  info = "across() correctly names output columns for named lists of functions and custom names"
)
expect_named(
  summarise(gf, across(where(is.numeric), list(mean = mean, sum))),
  c("x", "y_mean", "y_2", "z_mean", "z_2"),
  info = "across() correctly names output columns for partially named lists of functions"
)
expect_named(
  summarise(gf, across(where(is.numeric), list(mean, sum = sum))),
  c("x", "y_1", "y_sum", "z_1", "z_sum"),
  info = "across() correctly names output columns for partially named lists of functions"
)
expect_named(
  summarise(gf, across(where(is.numeric), list(mean, sum = sum),
                       .names = "my_col_{.col}_{.fn}")),
  c("x", "my_col_y_1", "my_col_y_sum", "my_col_z_1", "my_col_z_sum"),
  info = "across() correctly names output columns for partially named lists of functions and custom names"
)
expect_named(
  summarise(gf, across(where(is.numeric), list(mean, sum))),
  c("x", "y_1", "y_2", "z_1", "z_2"),
  info = "across() correctly names output columns for non-named lists of functions"
)
expect_named(
  summarise(gf, across(where(is.numeric), list(mean, sum), .names = "my_col_{.col}_{.fn}")),
  c("x", "my_col_y_1", "my_col_y_2", "my_col_z_1", "my_col_z_2"),
  info = "across() correctly names output columns for non-named lists of functions and custom names"
)

expect_identical(
  data.frame(x = 1:2, y = c("a", "b")) %>% summarise(across(everything(), list(cls = class, type = is.numeric))),
  data.frame(x_cls = "integer", x_type = TRUE, y_cls = "character", y_type = FALSE),
  info = "across() result locations are aligned with column names"
)

expect_equal(
  summarise(data.frame(x = c(1, NA)), across(everything(), mean, na.rm = TRUE)),
  data.frame(x = 1),
  info = "across() passes ... to functions"
)

expect_equal(
  summarise(data.frame(x = c(1, NA)), across(everything(), list(mean = mean, median = median), na.rm = TRUE)),
  data.frame(x_mean = 1, x_median = 1),
  info = "across() passes ... to functions"
)

df <- data.frame(x = 1)
expect_equal(
  mutate(df, across(x, `+`, 1)),
  data.frame(x = 2),
  info = "across() passes unnamed arguments following .fns as ..."
)

expect_equal(
  summarize(data.frame(x = c(1, 2)), across(x, tail, n = 1)),
  data.frame(x = 2),
  info = "across() avoids simple argument name collisions with ..."
)

df <- data.frame(a = 1)
expect_equal(
  mutate(df, x = ncol(across(where(is.numeric))), y = ncol(across(where(is.numeric)))),
  data.frame(a = 1, x = 1L, y = 2L),
  info = "across() works sequentially"
)
expect_equal(
  mutate(df, a = "x", y = ncol(across(where(is.numeric)))),
  data.frame(a = "x", y = 0L),
  info = "across() works sequentially"
)
expect_equal(
  mutate(df, x = 1, y = ncol(across(where(is.numeric)))),
  data.frame(a = 1, x = 1, y = 2L),
  info = "across() works sequentially"
)

expect_named(
  mutate(data.frame(a = 1, b = 2), a = 2, x = across())$x,
  c("a", "b"),
  info = "across() retains original ordering"
)

expect_equal(
  mutate(data.frame(a = 1, b = 2), x = ncol(across(where(is.numeric))) + ncol(across(a))),
  data.frame(a = 1, b = 2, x = 3),
  info = "across() can be used twice in the same expression"
)

expect_equal(
  mutate(data.frame(a = 1, b = 2), x = ncol(across(where(is.numeric))), y = ncol(across(a))),
  data.frame(a = 1, b = 2, x = 2, y = 1),
  info = "across() can be used in separate expressions"
)

df <- data.frame(g = 1:2, a = 1:2, b = 3:4) %>% group_by(g)
expect_equal(
  mutate(df, x = if_else(cur_group_id() == 1L, across(a)$a, across(b)$b)),
  structure(
    list(g = 1:2, a = 1:2, b = 3:4, x = c(1L, 4L)),
    groups = structure(list(g = 1:2, .rows = list(1L, 2L)),
      row.names = 1:2,
      class = "data.frame",
      .drop = TRUE
    ), row.names = 1:2, class = c("grouped_df", "data.frame")
  ),
  info = "across() usage can depend on the group id"
)

df <- data.frame(g = rep(1:2, each = 2), a = 1:4) %>% group_by(g)
expect_identical(
  mutate(df, data.frame(x = across(where(is.numeric), mean)$a, y = across(where(is.numeric), max)$a)),
  mutate(df, x = mean(a), y = max(a)),
  info = "across() internal cache key depends on all inputs"
)

# The selection finds objects in the environment where across() is called, e.g. inside a function
df <- data.frame(g = c(1, 1, 2), x = c(3, 1, 2), y = c(10, 20, 30), z = c("a", "b", "c"))
local_selections <- function(df) {
  cols <- c("x", "y")
  is_big <- function(v) is.numeric(v) && max(v) > 5
  pfx <- "y"
  list(
    summarise = summarise(df, across(all_of(cols), sum)),
    grouped = summarise(group_by(df, g), across(all_of(cols), sum)),
    where = mutate(df, across(where(is_big), ~ .x * 2)),
    helper = mutate(df, across(starts_with(pfx), ~ .x + 1)),
    bare = summarise(df, across(cols, sum)),
    if_any = filter(df, if_any(all_of(cols), ~ .x > 15)),
    if_all = filter(group_by(df, g), if_all(all_of(cols), ~ .x > 1)),
    arrange = arrange(df, across(all_of(cols), desc))
  )
}
res <- local_selections(df)
expect_equal(
  res$summarise, data.frame(x = 6, y = 60),
  info = "across() finds a vector given to all_of() in the calling function"
)
expect_equal(
  ungroup(res$grouped), data.frame(g = c(1, 2), x = c(4, 2), y = c(30, 30)),
  info = "across() finds a vector given to all_of() in the calling function in grouped data"
)
expect_equal(
  res$where, data.frame(g = c(1, 1, 2), x = c(3, 1, 2), y = c(20, 40, 60), z = c("a", "b", "c")),
  info = "across() finds a predicate given to where() in the calling function"
)
expect_equal(
  res$helper, data.frame(g = c(1, 1, 2), x = c(3, 1, 2), y = c(11, 21, 31), z = c("a", "b", "c")),
  info = "across() finds a string given to a selection helper in the calling function"
)
expect_equal(
  res$bare, data.frame(x = 6, y = 60),
  info = "across() finds a bare vector of names in the calling function"
)
expect_equal(
  res$if_any, data.frame(g = c(1, 2), x = c(1, 2), y = c(20, 30), z = c("b", "c"), row.names = 2:3),
  info = "if_any() finds a vector given to all_of() in the calling function"
)
expect_equal(
  ungroup(res$if_all), data.frame(g = c(1, 2), x = c(3, 2), y = c(10, 30), z = c("a", "c"), row.names = c(1L, 3L)),
  info = "if_all() finds a vector given to all_of() in the calling function in grouped data"
)
expect_equal(
  res$arrange, data.frame(g = c(1, 2, 1), x = c(3, 2, 1), y = c(10, 30, 20), z = c("a", "c", "b")),
  info = "across() in arrange() finds a vector given to all_of() in the calling function"
)

# The columns are selected once for each expression, and where() tests the full columns rather than those of a group
expect_equal(
  ungroup(summarise(
    group_by(data.frame(g = c(1, 1, 2, 2), x = c(1, 2, 10, 20), y = c(5, 5, 0, 0)), g),
    across(where(function(v) mean(v) > 3), sum)
  )),
  data.frame(g = c(1, 2), x = c(3, 30)),
  info = "across() selects the same columns in every group, by testing the full columns in where()"
)
calls <- 0L
counting <- function(v) {
  calls <<- calls + 1L
  is.numeric(v)
}
invisible(summarise(group_by(data.frame(g = rep(1:5, 2), x = 1:10, y = 1:10), g), across(where(counting), sum)))
expect_true(calls <= 3L, info = "across() tests each column once in where(), not once for each group")

gdf <- group_by(data.frame(g = c(2, 1, 2, 1), x = c(3, 5, 7, 1), y = c(10, 20, 30, 40)), g)
expect_equal(
  ungroup(mutate(gdf, x = x * 100, across(where(function(v) mean(v) > 300), ~ 0))),
  data.frame(g = c(2, 1, 2, 1), x = c(0, 0, 0, 0), y = c(10, 20, 30, 40)),
  info = "where() in mutate() tests the full column made by an earlier expression"
)
expect_equal(
  summarise(gdf, s = sum(x), across(where(function(v) all(v > 5)), max)),
  data.frame(g = c(1, 2), s = c(6, 10), y = c(40, 30)),
  info = "where() in summarise() tests the summaries of every group made by an earlier expression"
)
expect_equal(
  ungroup(mutate(gdf, across(where(function(v) v[1] == 3), ~ -.x))),
  data.frame(g = c(2, 1, 2, 1), x = c(-3, -5, -7, -1), y = c(10, 20, 30, 40)),
  info = "where() tests the columns in the order of the rows, not of the groups"
)
expect_equal(
  ungroup(mutate(
    gdf, s = sum(x), k = 1,
    across(where(function(v) identical(v, c(10, 6, 10, 6)) || identical(v, c(1, 1, 1, 1))), ~ .x * 0)
  )),
  data.frame(g = c(2, 1, 2, 1), x = c(3, 5, 7, 1), y = c(10, 20, 30, 40), s = c(0, 0, 0, 0), k = c(0, 0, 0, 0)),
  info = "where() in mutate() tests the full columns of earlier results, recycled to the rows and in their order"
)
expect_equal(
  ungroup(summarise(gdf, s = range(x), across(where(function(v) identical(v, c(1, 5, 3, 7))), ~ .x * 10))),
  data.frame(g = c(1, 1, 2, 2), s = c(10, 50, 30, 70)),
  info = "where() in summarise() tests the results of every group, in the order of the groups"
)

across_x <- function(fn) across(x, fn)
expect_equal(
  ungroup(mutate(gdf, data.frame(a = across_x(min)$x, b = across_x(max)$x))),
  data.frame(g = c(2, 1, 2, 1), x = c(3, 5, 7, 1), y = c(10, 20, 30, 40), a = c(3, 1, 3, 1), b = c(7, 5, 7, 5)),
  info = "across() called from a function in the expression uses the functions of each call"
)
expect_equal(
  ungroup(mutate(gdf, z = across(x, if (cur_group_id() == 1L) ~ .x * 10 else ~ .x * 100)$x)),
  data.frame(g = c(2, 1, 2, 1), x = c(3, 5, 7, 1), y = c(10, 20, 30, 40), z = c(300, 50, 700, 10)),
  info = "across() uses the functions it is given in each group"
)
expect_equal(
  ungroup(mutate(gdf, across(x, ~ .x + y))),
  data.frame(g = c(2, 1, 2, 1), x = c(13, 25, 37, 41), y = c(10, 20, 30, 40)),
  info = "the functions of across() see the columns of their own group"
)
expect_equal(
  ungroup(mutate(gdf, across(x, function(v) v + y), s = cur_data()$x)),
  data.frame(g = c(2, 1, 2, 1), x = c(13, 25, 37, 41), y = c(10, 20, 30, 40), s = c(13, 25, 37, 41)),
  info = "a function made in the expression sees the columns of its own group, and later expressions see the results"
)
expect_equal(
  ungroup(mutate(gdf, n = ncol(summarise(data.frame(p = 1, q = 2), across(everything(), sum))), across(x, ~ .x * n))),
  data.frame(g = c(2, 1, 2, 1), x = c(6, 10, 14, 2), y = c(10, 20, 30, 40), n = c(2L, 2L, 2L, 2L)),
  info = "across() in a verb inside an expression keeps the selections of the outer verb apart"
)
expect_equal(
  summarise(gdf, across(c(x, y), list(lo = min, hi = max), .names = "{.fn}_{toupper(.col)}")),
  data.frame(g = c(1, 2), lo_X = c(1, 3), hi_X = c(5, 7), lo_Y = c(20, 10), hi_Y = c(40, 30)),
  info = "across() fills in .names templates with expressions of .col and .fn"
)

# The results of the groups are bound column by column
gdf <- group_by(data.frame(g = c(2, 1, 2, 1), x = c(3L, 5L, 7L, 1L), f = factor(c("p", "q", "p", "r"))), g)
expect_identical(
  ungroup(mutate(gdf, across(x, ~ if (cur_group_id() == 1L) .x else .x / 2))),
  data.frame(g = c(2, 1, 2, 1), x = c(1.5, 5, 3.5, 1), f = factor(c("p", "q", "p", "r"))),
  info = "across() results of different numeric types are combined across groups and put back in the order of the rows"
)
expect_identical(
  summarise(gdf, across(c(x, f), ~ .x[1])),
  data.frame(g = c(1, 2), x = c(5L, 3L), f = factor(c("q", "p"), levels = c("p", "q", "r"))),
  info = "across() results with factors are combined across groups"
)

expect_error(
  data.frame(x = 1) %>% summarise(across(everything(), "foo")),
  info = "across() fails on non-function"
)

expect_equal(
  mutate(data.frame(), across()),
  data.frame(),
  info = "across() works with empty data.frames"
)

# Formula approach

expect_equal(
  data.frame(x = 1, y = 2) %>% summarise(across(everything(), ~rep(42, .))),
  data.frame(x = rep(42, 2), y = rep(42, 2)),
  info = "across() uses tidy recycling rules"
)

expect_error(
  data.frame(x = 2, y = 3) %>% summarise(across(everything(), ~rep(42, .))),
  info = "across() uses tidy recycling rules #2"
)

expect_equal(
  iris %>% group_by(Species) %>% summarise(across(starts_with("Sepal"), ~mean(., na.rm = TRUE))) %>% ungroup(),
  data.frame(
    Species = structure(1:3, .Label = c("setosa", "versicolor", "virginica"), class = "factor"),
    Sepal.Length = c(5.006, 5.936, 6.588),
    Sepal.Width = c(3.428, 2.77, 2.974)
  ),
  info = "purrr style formulas work"
)

expect_equal(
  mtcars %>% summarise(across(starts_with("m"), list(~mean(.x), ~sd(.x)))),
  data.frame(mpg_1 = mean(mtcars$mpg), mpg_2 = sd(mtcars$mpg)),
  info = "list of purrr style formulas works"
)

expect_equal(
  mtcars %>% summarise(across(starts_with("m"), list("mean", "sd"))),
  data.frame(mpg_1 = mean(mtcars$mpg), mpg_2 = sd(mtcars$mpg)),
  info = "character vector style functions work"
)

# if_all() and if_any()

d <- data.frame(x = 10, y = 10)
expect_error(filter(d, if_all(x:y, identity)), info = "if_all() enforces logical in filter")
expect_error(filter(d, if_any(x:y, identity)), info = "if_any() enforces logical in filter")
expect_error(mutate(d, ok = if_all(x:y, identity)), info = "if_all() enforces logical in mutate")
expect_error(mutate(d, ok = if_any(x:y, identity)), info = "if_any() enforces logical in mutate")

d <- data.frame(x = c(1, 5, 10, 10), y = c(0, 0, 0, 10), z = c(10, 5, 1, 10))
res <- mutate(.data = d, any = if_any(x:z, ~ . > 8), all = if_all(x:z, ~ . > 8))
expect_equal(res$any, c(TRUE, FALSE, TRUE, TRUE), info = "if_any() can be used in mutate")
expect_equal(res$all, c(FALSE, FALSE, FALSE, TRUE), info = "if_all() can be used in mutate")

df <- expand.grid(
  x = c(TRUE, FALSE, NA), y = c(TRUE, FALSE, NA)
)
expect_identical(
  filter(df, x & y),
  filter(df, if_all(c(x, y), identity)),
  info = "if_all() respects filter()-like NA handling"
)
expect_identical(
  filter(df, x | y),
  filter(df, if_any(c(x, y), identity)),
  info = "if_any() respects filter()-like NA handling"
)

expect_equal(
  mtcars %>% filter(if_any(contains("Width"), ~ . > 4)),
  mtcars,
  info = "if_any() without columns keeps every row in filter()"
)
expect_equal(
  mtcars %>% filter(if_all(contains("Width"), ~ . > 4)),
  mtcars,
  info = "if_all() without columns keeps every row in filter()"
)
expect_equal(
  nrow(filter(group_by(mtcars, cyl), if_any(contains("Width"), ~ . > 4), mpg > 30)),
  4L,
  info = "if_any() without columns keeps every row in grouped filter(), combined with other conditions"
)
expect_equal(
  mutate(data.frame(x = 1:3), k = if_any(starts_with("z"), ~ .x > 1), m = if_all(starts_with("z"), ~ .x > 1)),
  data.frame(x = 1:3, k = TRUE, m = TRUE),
  info = "if_any() and if_all() without columns give TRUE in mutate()"
)
expect_equal(
  summarise(group_by(data.frame(g = c("b", "a", "b"), x = 1:3), g), k = if_all(starts_with("z"), ~ .x > 1)),
  data.frame(g = c("a", "b"), k = TRUE),
  info = "if_all() without columns gives TRUE in grouped summarise()"
)

# As in dplyr, the grouping columns are not part of the columns that across() selects from, so positions count the
# other columns and a grouping column cannot be selected
gdf <- group_by(data.frame(g = c(1, 1, 2), x = 1:3, y = c(10, 20, 30), z = c("a", "b", "c")), g)
expect_equal(
  summarise(gdf, across(1, sum)),
  data.frame(g = c(1, 2), x = c(3L, 3L)),
  info = "across() counts positions without the grouping columns"
)
expect_equal(
  summarise(gdf, across(1:2, sum)),
  data.frame(g = c(1, 2), x = c(3L, 3L), y = c(30, 30)),
  info = "across() selects a range of positions without the grouping columns"
)
expect_equal(
  summarise(gdf, across(-1, ~ .x[1])),
  data.frame(g = c(1, 2), y = c(10, 30), z = c("a", "c")),
  info = "across() deselects positions without the grouping columns"
)
expect_error(summarise(gdf, across(g, sum)), info = "across() cannot select a grouping column by name")
expect_error(summarise(gdf, across("g", sum)), info = "across() cannot select a grouping column by string")
expect_error(summarise(gdf, across(4, sum)), info = "across() counts the columns without the grouping columns")
expect_equal(
  ungroup(mutate(gdf, w = x * 2, across(4, ~ .x + 1000))),
  data.frame(g = c(1, 1, 2), x = 1:3, y = c(10, 20, 30), z = c("a", "b", "c"), w = c(1002, 1004, 1006)),
  info = "across() in mutate() counts the columns made by earlier expressions after the other columns"
)
expect_equal(
  ungroup(filter(gdf, if_any(1, ~ .x > 1))),
  data.frame(g = c(1, 2), x = 2:3, y = c(20, 30), z = c("b", "c"), row.names = 2:3),
  info = "if_any() counts positions without the grouping columns"
)
# Unlike dplyr 1.0.10, where group_cols() fails inside across(), it selects nothing there, so -group_cols() selects
# every column
expect_equal(
  summarise(gdf, across(-group_cols(), ~ .x[1])),
  data.frame(g = c(1, 2), x = c(1L, 3L), y = c(10, 30), z = c("a", "c")),
  info = "across(-group_cols()) selects every column but the grouping columns"
)
expect_equal(
  summarise(gdf, across(group_cols(), ~ .x[1])),
  data.frame(g = c(1, 2)),
  info = "across(group_cols()) selects no columns"
)
expect_equal(
  summarise(data.frame(x = 1:3, y = 4:6), across(-group_cols(), max)),
  data.frame(x = 3L, y = 6L),
  info = "across(-group_cols()) selects every column of ungrouped data"
)
expect_equal(
  summarise(
    gdf, across(where(function(v) if (identical(v, c(1, 1, 2))) stop("tested g") else is.numeric(v)), sum)
  ),
  data.frame(g = c(1, 2), x = c(3L, 3L), y = c(30, 30)),
  info = "where() in across() does not test the grouping columns"
)
expect_equal(
  summarise(gdf, across(where(~ is.numeric(.x) && mean(.x) > 3), sum)),
  data.frame(g = c(1, 2), y = c(30, 30)),
  info = "where() in across() accepts a formula"
)

# Lambdas made in the expression are converted once and given the environment of each group
expect_equal(
  ungroup(mutate(gdf, across(c(x, y), list(a = ~ .x + n(), b = ~ .x * cur_group_id())))),
  data.frame(
    g = c(1, 1, 2), x = 1:3, y = c(10, 20, 30), z = c("a", "b", "c"),
    x_a = c(3, 4, 4), x_b = c(1, 2, 6), y_a = c(12, 22, 31), y_b = c(10, 20, 60)
  ),
  info = "a lambda in across() sees the data of each group"
)
k <- 0
expect_equal(
  ungroup(mutate(gdf, across(x, ~ .x + max(y) + k))),
  data.frame(g = c(1, 1, 2), x = c(21, 22, 33), y = c(10, 20, 30), z = c("a", "b", "c")),
  info = "a lambda in across() sees the columns of each group and the calling environment"
)
expect_equal(
  ungroup(mutate(gdf, across(x, if (cur_group_id() == 1L) list(~ .x * 10) else list(~ .x * 100)))),
  data.frame(g = c(1, 1, 2), x = c(10, 20, 300), y = c(10, 20, 30), z = c("a", "b", "c")),
  info = "a lambda in across() which differs between groups is used in its group"
)
rm(gdf, k)
