expect_error(
  mtcars %>% group_by(fake_column_name)
)

res <- suppressMessages(mtcars %>% group_by(am, cyl) %>% select(mpg))
expect_equal(
  colnames(res),
  c("am", "cyl", "mpg"),
  info = "Test that groups persist in select() when no groups are selected"
)

res <- suppressMessages(mtcars %>% group_by(am, cyl) %>% select(mpg, cyl))
expect_equal(
  colnames(res),
  c("am", "mpg", "cyl"),
  info = "Test that groups persist in select() when only some groups are selected"
)

res <- mtcars %>% group_by(am, cyl) %>% relocate(gear, .before = mpg)
expect_equal(
  colnames(res),
  c("gear", "mpg", "cyl", "disp", "hp", "drat", "wt", "qsec", "vs", "am", "carb"),
  info = "Test that groups persist in relocate()"
)

res <- mtcars %>% group_by(am, cyl) %>% rename(Gears = gear)
expect_equal(
  colnames(res),
  c("mpg", "cyl", "disp", "hp", "drat", "wt", "qsec", "vs", "am", "Gears", "carb"),
  info = "Test that groups persist in rename()"
)

res <- mtcars %>% group_by(tmp = am * cyl)
expect_equal(
  colnames(res),
  c("mpg", "cyl", "disp", "hp", "drat", "wt", "qsec", "vs", "am", "gear", "carb", "tmp"),
  info = "group_by() can create new columns #1"
)
expect_equal(
  res$tmp,
  mtcars$am * mtcars$cyl,
  info = "group_by() can create new columns #2"
)

# NULL group
expect_equal(
  group_by(mtcars, NULL),
  mtcars,
  info = "NULL group returns the original data.frame"
)
res <- group_by(mtcars, am, cyl)
expect_equal(
  class(group_by(res, NULL)),
  "data.frame",
  info = "group_by(., NULL) ungroups data #1"
)
expect_equal(
  attr(group_by(res, NULL), "groups", exact = TRUE),
  NULL,
  info = "group_by(., NULL) ungroups data #2"
)

rm(res)

# .drop = TRUE ---------------------------------------------------

res <- iris %>%
  filter(Species == "setosa") %>%
  group_by(Species, .drop = TRUE)
expect_identical(
  group_data(res),
  structure(
    list(
      Species = structure(1L, .Label = c("setosa", "versicolor",  "virginica"), class = "factor"),
      .rows = list(1:50)
    ),
    row.names = 1L, class = "data.frame", .drop = TRUE
  ),
  info = "group_by(.drop = TRUE) drops empty groups"
)
expect_true(group_by_drop_default(res))


res <- iris %>%
  filter(Species == "setosa") %>%
  group_by(Species, .drop = TRUE)
res2 <- filter(res, Sepal.Length > 5)
expect_true(group_by_drop_default(res2), info = "grouped data.frames remember their .drop")

res3 <- filter(res, Sepal.Length > 5, .preserve = FALSE)
expect_true(group_by_drop_default(res3), info = "grouped data.frames remember their .drop")

res4 <- group_by(res3, Species)
expect_true(group_by_drop_default(res4), info = "grouped data.frames remember their .drop")
expect_equal(nrow(group_data(res4)), 1L, info = "grouped data.frames remember their .drop")

res <- iris %>%
  filter(Species == "setosa") %>%
  group_by(Species, .drop = FALSE)
expect_false(group_by_drop_default(res), info = "grouped data frames remember their .drop = FALSE")

res2 <- res %>%
  group_by(Species)
expect_false(group_by_drop_default(res2), info = "grouped data frames remember their .drop = FALSE")

df <- data.frame(x = ordered("x"))
drop <- df %>% group_by(x) %>% group_data()
nodrop <- df %>% group_by(x, .drop = FALSE) %>% group_data()
expect_equal(is.ordered(drop$x), is.ordered(nodrop$x), info = "group_by(.drop = FALSE) preserve ordered factors")
expect_true(is.ordered(nodrop$x), info = "group_by(.drop = FALSE) preserve ordered factors")

df <- data.frame(
  f1 = factor("a", levels = c("a", "b", "c")),
  f2 = factor("d", levels = c("d", "e", "f", "g")),
  x = 42
)

res <- df %>%
  group_by(f1, f2, .drop = TRUE)
expect_equal(n_groups(res), 1L, info = "summarise maintains the .drop attribute")

res2 <- summarise(res, x = sum(x))
expect_equal(n_groups(res2), 1L, info = "summarise maintains the .drop attribute")
expect_true(group_by_drop_default(res2), info = "summarise maintains the .drop attribute")

df1 <- group_by(data.frame(
  f1 = factor(c("a", "b"), levels = c("a", "b", "c")),
  x  = 42:43
), f1, .drop = TRUE)
df2 <- group_by(data.frame(
  f1 = factor(c("a"), levels = c("a", "b", "c")),
  y = 1
), f1, .drop = TRUE)
res <- left_join(df1, df2, by = "f1")
expect_equal(n_groups(res), 2L, info = "joins maintain the .drop attribute")

df2 <- group_by(data.frame(
  f1 = factor(c("a", "c"), levels = c("a", "b", "c")),
  y = 1:2
), f1, .drop = TRUE)
res <- full_join(df1, df2, by = "f1")
expect_equal(n_groups(res), 3L, info = "joins maintain the .drop attribute")

d <- data.frame(
  f1 = factor("b", levels = c("a", "b", "c")),
  f2 = factor("g", levels = c("e", "f", "g")),
  x  = 48
)
res <- group_by(group_by(d, f1, .drop = TRUE), f2, .add = TRUE)
expect_equal(n_groups(res), 1L, info = "group_by(add = TRUE) sets .drop if the origonal data was .drop = TRUE")
expect_true(group_by_drop_default(res), info = "group_by(add = TRUE) sets .drop if the origonal data was .drop = TRUE")

df <- data.frame(x = 1:2, y = 1:2) %>%
  structure(class = c("grouped_df", "data.frame"))
expect_true(group_by_drop_default(df), info = "group_by_drop_default() is forgiving about corrupt grouped df")

res <- data.frame(x = c("apple", NA, "banana"), y = 1:3, stringsAsFactors = FALSE) %>%
  group_by(x) %>%
  group_data()
expect_identical(res$x, c("apple", "banana", NA_character_), info = "group_by() puts NA groups last in STRSXP")
expect_identical(res$.rows, list(1L, 3L, 2L), info = "group_by() puts NA groups last in STRSXP")

# Grouping keys -----------------------------------------------------------------

df <- data.frame(x = c("a.b", "a"), y = c("c", "b.c"), v = 1:2, stringsAsFactors = FALSE)
expect_equal(
  group_rows(group_by(df, x, y)),
  list(2L, 1L),
  info = "group_by() keeps groups apart when their pasted values are the same"
)

df <- data.frame(g = c("a", NA, "a", NA), h = c(1, 1, 2, 2), stringsAsFactors = FALSE)
expect_equal(
  group_rows(group_by(df, g)),
  list(c(1L, 3L), c(2L, 4L)),
  info = "group_by() puts missing values in their own group, which comes last"
)
expect_equal(
  group_rows(group_by(df, h, g)),
  list(1L, 2L, 3L, 4L),
  info = "group_by() keeps missing values in different groups apart"
)

df <- data.frame(x = c(0.1 + 0.2, 0.3))
expect_equal(
  group_rows(group_by(df, x)),
  list(2L, 1L),
  info = "group_by() compares numeric values exactly"
)

df <- data.frame(
  f1 = factor(c("a", "b"), levels = c("a", "b", "c")),
  f2 = factor(c("x", "y"))
)
res <- group_data(group_by(df, f1, f2, .drop = FALSE))
expect_equal(
  data.frame(f1 = as.character(res$f1), f2 = as.character(res$f2), stringsAsFactors = FALSE),
  data.frame(f1 = rep(c("a", "b", "c"), each = 2), f2 = rep(c("x", "y"), 3), stringsAsFactors = FALSE),
  info = "group_by(.drop = FALSE) includes every combination of factor levels"
)
expect_equal(
  res$.rows,
  list(1L, integer(), integer(), 2L, integer(), integer()),
  info = "group_by(.drop = FALSE) gives empty combinations no rows"
)

df <- data.frame(x = ordered(c("a", "b"), levels = c("a", "b", "c")))
expect_true(
  is.ordered(group_data(group_by(df, x, .drop = FALSE))$x),
  info = "group_by(.drop = FALSE) preserves ordered factors when levels are unused"
)

df <- data.frame(a = seq_len(500), b = rev(seq_len(500)), c = rep(1:5, 100))
expect_equal(
  n_groups(group_by(df, a, b, c)),
  500L,
  info = "group_by() does not build every combination of several high cardinality columns"
)

rm(df, res)

gb_df <- data.frame(x = 1:2, y = c(10, 20))
res <- group_by(gb_df, x + y + x + y + x + y + x + y + x + y + x + y + x + y + x + y + x + y + x)
expect_equal(
  list(names(res), group_vars(res)),
  list(c("x", "y", "... + x"), "... + x"),
  info = "group_by() abbreviates long automatic names"
)

ge_df <- data.frame(x = 1:4, g = c("a", "a", "b", "b"))
ge_nm <- "tot"
res <- group_by(ge_df, tot = g)
expect_equal(
  list(res$tot, group_vars(res)),
  list(c("a", "a", "b", "b"), "tot"),
  info = "group_by() groups by a renamed column"
)
expect_equal(
  group_vars(group_by(ge_df, z := g, "{ge_nm}" := x)), # nolint: object_name_linter.
  c("z", "tot"),
  info = "group_by() supports :="
)
res <- group_by(ge_df, 5, "lit")
expect_equal(
  list(res[["5"]], res[["\"lit\""]], group_vars(res)),
  list(rep(5, 4L), rep("lit", 4L), c("5", "\"lit\"")),
  info = "group_by() groups by a literal, named after its code"
)
expect_equal(
  group_by(ge_df, .data$g),
  group_by(ge_df, g),
  info = "group_by() groups by .data$g as the column g"
)
res <- group_by(group_by(ge_df, g), h = x, .add = TRUE)
expect_equal(list(res$h, group_vars(res)), list(1:4, c("g", "h")), info = "group_by(.add = TRUE) adds a renamed column")
ge_fn <- function(data) {
  k <- 2L
  group_by(data, h = x %/% k)
}
expect_equal(ge_fn(ge_df)$h, c(0L, 1L, 1L, 2L), info = "group_by() finds variables in the calling environment")

fe_df <- data.frame(g = c(1, 1, 2), x = 1:3)
fe_k <- 100L
fe_inner <- function(data) {
  off <- 10L
  mutate(data, y = x + off)$y[1L]
}
expect_equal(
  group_by(fe_df, n = fe_inner(fe_df))$n,
  rep(11L, 3L),
  info = "a mutate() called inside group_by() finds variables in its own calling environment"
)
fe_fn <- function(data) {
  fe_k <- 2L
  nm <- "new"
  list(
    group_by(data, h = x %% fe_k)$h,
    names(group_by(data, "{nm}" := x)) # nolint: object_name_linter.
  )
}
expect_equal(
  fe_fn(fe_df),
  list(c(1L, 0L, 1L), c("g", "x", "new")),
  info = "group_by() uses a local variable over a global one, also in a := template"
)

# A verb which groups its own `...` calls the `group_by()` generic, so that a method for another class is used
gd_calls <- 0L
registerS3method("group_by", "gd_class", function(.data, ...) {
  gd_calls <<- gd_calls + 1L
  NextMethod()
}, envir = asNamespace("poorman"))
gd_df <- structure(data.frame(g = c(1, 1, 2), x = 1:3), class = c("gd_class", "data.frame"))
gd_res <- vapply(
  list(
    function() count(gd_df, g),
    function() add_count(gd_df, g),
    function() group_split(gd_df, g),
    function() nest_by(gd_df, g)
  ),
  function(f) {
    gd_calls <<- 0L
    f()
    gd_calls > 0L
  },
  NA
)
expect_true(all(gd_res), info = "count(), add_count(), group_split() and nest_by() call group_by() methods")

# A verb which groups its own `...` finds variables in the frame of its caller, also when it is reached through a
# method which forwards to it with `NextMethod()` or through a function which passes `...` on to it
registerS3method("nest_by", "gw_class", function(.data, ...) NextMethod(), envir = asNamespace("poorman"))
registerS3method("group_split", "gw_class", function(.data, ...) NextMethod(), envir = asNamespace("poorman"))
gw_df <- structure(data.frame(x = 1:4), class = c("gw_class", "data.frame"))
# nolint start: object_usage_linter. `x` is a column, not a global variable.
gw_fn <- function(data) {
  k <- 2L
  wrap <- function(d, ...) add_count(d, ...)
  list(
    nrow(nest_by(data, h = x %/% k)),
    length(group_split(data, h = x %/% k)),
    wrap(ungroup(data), h = x %/% k)$n
  )
}
# nolint end
expect_equal(
  gw_fn(gw_df),
  list(3L, 3L, c(1L, 2L, 2L, 1L)),
  info = "group_split(), nest_by() and add_count() find variables through a NextMethod() method or a wrapper"
)
