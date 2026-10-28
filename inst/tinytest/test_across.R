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
# Unlike dplyr 1.0.10, which evaluates the `.fns` of a call made directly in the expression once, before any group,
# `.fns` is evaluated in each group, as any other code of the expression is
expect_equal(
  ungroup(mutate(gdf, across(x, if (cur_group_id() == 1L) list(~ .x * 10) else list(~ .x * 100)))),
  data.frame(g = c(1, 1, 2), x = 1:3, y = c(10, 20, 30), z = c("a", "b", "c"), x_1 = c(10, 20, 300)),
  info = "a lambda in across() which differs between groups is used in its group"
)
expect_equal(
  summarise(group_by(data.frame(g = c(1, 1, 2, 2, 3), x = 1:5), g), across(x, if (cur_group_id() == 1L) min else max)),
  data.frame(g = c(1, 2, 3), x = c(1L, 4L, 5L)),
  info = "the .fns of across() is evaluated in each group"
)
rm(gdf, k)

# A call made from a function is selected again each time it is evaluated, as its selection can use objects of that
# function which change between calls
sum_cols <- function(cols) {
  s <- 0
  for (v in cols) s <- s + across(all_of(v))[[1]]
  s
}
gdf <- group_by(data.frame(g = c(1, 1, 2, 2, 3), x = 1:5, y = c(10, 20, 30, 40, 50)), g)
expect_equal(
  summarise(gdf, s = sum(sum_cols(c("x", "y")))),
  data.frame(g = c(1, 2, 3), s = c(33, 77, 55)),
  info = "across() repeated in a loop of a function selects each column in grouped summarise()"
)
expect_equal(
  mutate(data.frame(x = 1:2, y = c(10, 20)), s = sum_cols(c("x", "y"))),
  data.frame(x = 1:2, y = c(10, 20), s = c(11, 22)),
  info = "across() repeated in a loop of a function selects each column in mutate()"
)
rm(gdf, sum_cols)

# A call made from a helper function is set up in every group, so it must still test the full columns, including those
# made by earlier expressions, whatever the number of groups
sum_numeric <- function() across(where(is.numeric), sum)
gdf <- group_by(data.frame(g = rep(1:500, each = 2), x = 1:1000), g)
expect_equal(
  summarise(gdf, m = mean(x), sum_numeric()),
  data.frame(g = 1:500, m = 1:500 * 2 - 0.5, x = 1:500 * 4L - 1L),
  info = "across() in a helper function works with many groups in summarise()"
)
add_one_to_large <- function() across(where(~ max(.x) > 300), ~ .x + 1)
gdf <- group_by(data.frame(g = rep(1:100, 2), x = 1:200), g)
expect_equal(
  ungroup(mutate(gdf, w = x * 2, add_one_to_large())),
  data.frame(g = rep(1:100, 2), x = 1:200, w = 1:200 * 2 + 1),
  info = "where() in a helper function tests the full column made by an earlier expression in mutate()"
)
rm(gdf, sum_numeric, add_one_to_large)

# The columns of the results of across() follow the same type rules across groups as the result of any expression
gdf <- group_by(data.frame(g = c(1, 1, 2), x = 1:3, y = 4:6), g)
expect_error(
  summarise(gdf, across(x, ~ if (g[1L] == 1) TRUE else "a")),
  paste0(
    "Caused by error:\n! `x` must return compatible vectors across groups.\n",
    "i Result type for group 1 (g = 1): <logical>.\ni Result type for group 2 (g = 2): <character>."
  ),
  fixed = TRUE,
  info = "across() gives an error for columns of incompatible types across groups in summarise()"
)
expect_error(
  mutate(gdf, across(c(x, y), ~ if (g[1L] == 2) as.character(.x) else .x)),
  paste0(
    "Caused by error:\n! `x` must return compatible vectors across groups.\n",
    "i Result type for group 1 (g = 1): <integer>.\ni Result type for group 2 (g = 2): <character>."
  ),
  fixed = TRUE,
  info = "across() gives an error for columns of incompatible types across groups in mutate()"
)
expect_error(
  summarise(gdf, across(c(x, y), list(a = ~ if (g[1L] == 2) as.raw(1) else 2L))),
  paste0(
    "Caused by error:\n! `x_a` must return compatible vectors across groups.\n",
    "i Result type for group 1 (g = 1): <integer>.\ni Result type for group 2 (g = 2): <raw>."
  ),
  fixed = TRUE,
  info = "The type error of across() names the result column"
)
expect_equal(
  summarise(gdf, across(x, ~ if (g[1L] == 1) NA else "a")),
  data.frame(g = c(1, 2), x = c(NA, "a")),
  info = "across() combines a column of only NA with a column of any type across groups"
)
expect_equal(
  summarise(gdf, across(x, ~ if (g[1L] == 1) 1L else 2.5)),
  data.frame(g = c(1, 2), x = c(1, 2.5)),
  info = "across() combines integer and double columns across groups"
)
rm(gdf)

# Each result of across() is one column, whatever its type, as in dplyr
df <- data.frame(g = c(1, 1, 2), x = 1:3, y = c(10, 20, 30))
gdf <- group_by(df, g)
expect_equal(
  summarise(gdf, across(x, ~ list(.x))),
  structure(list(g = c(1, 2), x = list(1:2, 3L)), row.names = c(NA, -2L), class = "data.frame"),
  info = "across() gives a list column for a function which returns a list in grouped summarise()"
)
expect_equal(
  summarise(df, across(x, ~ list(.x))),
  structure(list(x = list(1:3)), row.names = c(NA, -1L), class = "data.frame"),
  info = "across() gives a list column for a function which returns a list in summarise()"
)
expect_equal(
  summarise(gdf, across(c(x, y), list(l = ~ list(range(.x)), m = mean))),
  structure(
    list(g = c(1, 2), x_l = list(1:2, c(3L, 3L)), x_m = c(1.5, 3), y_l = list(c(10, 20), c(30, 30)), y_m = c(15, 30)),
    row.names = c(NA, -2L), class = "data.frame"
  ),
  info = "across() gives list columns next to other columns"
)
expect_equal(
  ungroup(mutate(gdf, across(x, ~ as.list(.x)))),
  structure(
    list(g = c(1, 1, 2), x = list(1L, 2L, 3L), y = c(10, 20, 30)), row.names = c(NA, -3L), class = "data.frame"
  ),
  info = "across() gives a list column for a function which returns a list in mutate()"
)
expect_equal(
  summarise(gdf, across(x, ~ data.frame(a = .x[1], b = .x[1] * 2))),
  structure(
    list(g = c(1, 2), x = structure(list(a = c(1L, 3L), b = c(2, 6)), row.names = c(NA, -2L), class = "data.frame")),
    row.names = c(NA, -2L), class = "data.frame"
  ),
  info = "across() gives a data frame column for a function which returns a data frame"
)
expect_equal(
  summarise(gdf, across(x, ~ matrix(.x[1:2], 1))),
  structure(list(g = c(1, 2), x = matrix(c(1L, 3L, 2L, NA), 2L)), row.names = c(NA, -2L), class = "data.frame"),
  info = "across() gives a matrix column for a function which returns a matrix"
)
rm(df, gdf)

# As in dplyr, an empty list of functions gives no columns, as an empty selection does
gdf <- group_by(data.frame(g = c(1, 1, 2), x = 1:3), g)
expect_equal(
  mutate(data.frame(x = 1:2), across(x, list())),
  data.frame(x = 1:2),
  info = "across() with an empty list of functions adds no columns in mutate()"
)
expect_silent(summarise(gdf, across(x, list())), info = "across() with an empty list of functions gives no warning")
expect_equal(
  summarise(gdf, across(x, list())),
  data.frame(g = c(1, 2)),
  info = "across() with an empty list of functions adds no columns in grouped summarise()"
)
rm(gdf)

# The columns of across() combine complex and double results across groups, as any other result does
gdf <- group_by(data.frame(g = c(1, 1, 2), x = 1:3), g)
expect_identical(
  summarise(gdf, across(x, ~ if (g[1L] == 1) 1i else 2))$x, c(1i, 2 + 0i),
  info = "across() combines complex and double columns across groups"
)
rm(gdf)

# As in dplyr, the results are named after the columns alone only for a single function which is not in a list
df <- data.frame(x = 1:3, y = c(1.5, 2.5, 3.5))
expect_named(summarise(df, across(x, list(mean))), "x_1", info = "across() names the result of a list of one function")
expect_named(
  summarise(df, across(c(x, y), list(~ mean(.x)))), c("x_1", "y_1"),
  info = "across() names the results of a list of one lambda"
)
expect_named(
  mutate(df, across(x, list(~ .x * 2))), c("x", "y", "x_1"),
  info = "across() in mutate() with a list of one function adds a column"
)
# As in dplyr, names are kept as they are, even when they are not syntactic
expect_named(
  summarise(df, across(x, list(mean, sd), .names = "{.fn}_{.col}")), c("1_x", "2_x"),
  info = "across() keeps names which start with a number"
)
expect_named(
  summarise(df, across(x, mean, .names = "mean of {.col}")), "mean of x",
  info = "across() keeps names with spaces"
)
expect_named(
  summarise(df, across(x, list(`my fn` = mean))), "x_my fn",
  info = "across() keeps function names with spaces"
)
expect_named(
  mutate(df, across(x, list(mean, sd), .names = "{.fn}")), c("x", "y", "1", "2"),
  info = "across() in mutate() keeps names which are numbers"
)
# As in dplyr 1.0.10, a template in .names is filled in without functions, as for one unnamed function
expect_named(
  summarise(df, across(x, NULL, .names = "new_{.col}")), "new_x",
  info = "across() without functions fills in {.col}"
)
expect_named(
  summarise(df, across(x, NULL, .names = "{.col}_{.fn}")), "x_1",
  info = "across() without functions fills in {.fn}"
)
rm(df)

# As in dplyr, the names of the results must be unique, as they are no longer repaired
df <- data.frame(y = 1:2, z = 3:4)
expect_error(
  summarise(df, across(c(y, z), list(a = sum, a = mean, a = max, b = min, b = sd))),
  paste0(
    "Names must be unique.\nx These names are duplicated:\n",
    "  * \"y_a\" at locations 1, 2, and 3.\n  * \"y_b\" at locations 4 and 5.\n",
    "  * \"z_a\" at locations 6, 7, and 8.\n  * \"z_b\" at locations 9 and 10."
  ),
  fixed = TRUE,
  info = "across() gives an error for duplicated names"
)
expect_error(
  mutate(group_by(df, z), across(y, list(a = sum, a = mean))),
  "Names must be unique.\nx These names are duplicated:\n  * \"y_a\" at locations 1 and 2.",
  fixed = TRUE,
  info = "across() gives an error for duplicated names in grouped mutate()"
)
expect_error(
  summarise(df, across(c(y, z), sum, .names = c("a", "a"))),
  "Names must be unique.",
  fixed = TRUE,
  info = "across() gives an error for duplicated names given as a vector"
)
# As in dplyr, a column which is selected without functions keeps its name
expect_named(
  summarise(data.frame(`my col` = 1:2, check.names = FALSE), across(`my col`)), "my col",
  info = "across() without functions keeps names with spaces"
)
rm(df)
