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
  mtcars[mtcars$mpg > 100, ],
  info = "if_any() conditions that return empty data.frames do not fail."
)
expect_equal(
  mtcars %>% filter(if_all(contains("Width"), ~ . > 4)),
  mtcars[mtcars$mpg > 100, ],
  info = "if_all() conditions that return empty data.frames do not fail."
)
