expect_equal(
  mtcars %>% summarise(mean(mpg)),
  data.frame("mean(mpg)" = mean(mtcars$mpg), check.names = FALSE),
  info = "Test single summaries"
)

expect_equal(
  mtcars %>% summarise(meanMpg = mean(mpg)),
  data.frame("meanMpg" = mean(mtcars$mpg)),
  info = "Test single summaries with column name"
)

expect_equal(
  mtcars %>% summarise(mean(mpg), sum(disp)),
  data.frame("mean(mpg)" = mean(mtcars$mpg), "sum(disp)" = sum(mtcars$disp), check.names = FALSE),
  info = "Test multiple summarise"
)

expect_equal(
  mtcars %>% summarise(meanMpg = mean(mpg), sumDisp = sum(disp)),
  data.frame("meanMpg" = mean(mtcars$mpg), "sumDisp" = sum(mtcars$disp), check.names = FALSE),
  info = "Test multiple summarise with column names"
)

expect_equal(
  summarise(data.frame(x = 1:10), y = mean(x), z = y + 1),
  data.frame(y = 5.5, z = 6.5),
  info = "Can use freshly create variables"
)

expect_equal(
  mtcars %>% summarise(n(), range(mpg)),
  structure(list(`n()` = c(32L, 32L), `range(mpg)` = c(10.4, 33.9)), class = "data.frame", row.names = c(NA, -2L)),
  info = "Functions returning multiple values get split over rows"
)

expect_equal(
  mtcars %>% summarise(n(), list(range(mpg))),
  structure(list(`n()` = 32L, `list(range(mpg))` = list(c(10.4, 33.9))), class = "data.frame", row.names = c(NA, -1L)),
  info = "Functions returning multiple values that are wrapped in list are returned as nested columns"
)

# Grouped Operations
res <- mtcars %>% group_by(am, cyl, gear) %>% summarise(meanMpg = mean(mpg), sumDisp = sum(disp))
gd <- group_data(res)
expect_equal(
  gd,
  structure(
    list(
      am = c(0, 0, 0, 1, 1, 1),
      cyl = c(4, 6, 8, 4, 6, 8),
      .rows = list(1:2, 3:4, 5L, 6:7, 8:9, 10L)
    ), row.names = c(NA, 6L), class = "data.frame", .drop = TRUE
  ),
  info = "Ensure the summarised data still contains group attributes"
)

attr(res, "groups") <- NULL
expect_equal(
  res,
  structure(
    list(
      am = c(0, 0, 0, 0, 0, 1, 1, 1, 1, 1),
      cyl = c(4, 4, 6, 6, 8, 4, 4, 6, 6, 8),
      gear = c(3, 4, 3, 4, 3, 4, 5, 4, 5, 5),
      meanMpg = c(21.5, 23.6, 19.75, 18.5, 15.05, 28.0333333333333, 28.2, 21, 19.7, 15.4),
      sumDisp = c(120.1, 287.5, 483, 335.2, 4291.4, 533.5, 215.4, 320, 145, 652)
    ),
    row.names = c(NA, 10L),
    class = c("grouped_df", "data.frame")
  ),
  info = "Test multiple groups and multiple summary functions"
)

expect_equal(
  summarise(mtcars),
  data.frame(row.names = 1L),
  info = "empty summarise returns a data.frame with one row and no columns"
)
expect_equal(
  dim(summarise(mtcars[0, ])),
  c(1L, 0L),
  info = "empty summarise of zero row data returns one row and no columns"
)
expect_equal(
  dim(summarise(data.frame(v = 1:2), a = NULL)),
  c(1L, 0L),
  info = "summarise with only NULL results returns one row and no columns"
)

res <- mtcars %>% group_by(am) %>% summarise()
expect_equal(
  class(res),
  "data.frame",
  info = "empty grouped summarise() returns groups #1"
)
expect_equal(
  {
    attr(res, "groups") <- NULL
    class(res) <- "data.frame"
    res
  },
  data.frame(am = c(0, 1)),
  info = "empty grouped summarise() returns groups #2"
)

# .groups
df <- data.frame(x = 1, y = 2) %>% group_by(x, y)
expect_equal(
  df %>% summarise() %>% group_vars(),
  "x",
  info = ".groups = NULL drops the last grouping variable when the results return a single value"
)
expect_equal(
  df %>% summarise(.groups = "drop_last") %>% group_vars(),
  "x",
  info = ".groups = 'drop_last' drops the last grouping variable"
)
expect_equal(
  df %>% summarise(.groups = "drop") %>% group_vars(),
  character(),
  info = ".groups = 'drop' drops all grouping variables"
)
expect_equal(
  df %>% summarise(.groups = "keep") %>% group_vars(),
  c("x", "y"),
  info = ".groups = 'keep' keeps all grouping variables"
)
expect_equal(
  mtcars %>%
    group_by(cyl, vs) %>%
    summarise(cyl_n = n()) %>%
    group_vars(),
  "cyl",
  info = ".groups = NULL drops the last grouping variable when the results return a single value for multiple groups"
)

# Grouped summarise -------------------------------------------------------------

df <- data.frame(g = c("a", NA, "a", NA), v = 1:4, stringsAsFactors = FALSE)
res <- summarise(group_by(df, g), s = sum(v), .groups = "drop")
expect_equal(
  res,
  data.frame(g = c("a", NA), s = c(4L, 6L), stringsAsFactors = FALSE),
  info = "summarise() keeps missing grouping values as their own group"
)
expect_equal(class(res), "data.frame", info = "summarise(.groups = 'drop') returns an ungrouped data.frame")

df <- data.frame(x = c("a.b", "a"), y = c("c", "b.c"), v = 1:2, stringsAsFactors = FALSE)
expect_equal(
  summarise(group_by(df, x, y), v = sum(v), .groups = "drop")$v,
  c(2L, 1L),
  info = "summarise() keeps groups apart when their pasted values are the same"
)

gf <- data.frame(g = c(1, 1, 2), x = 1:3) %>% group_by(g)
expect_equal(summarise(gf, f = sum(x))$f, c(3L, 3L), info = "summarise() on grouped data accepts a column called `f`")

df <- data.frame(g = character(0), v = integer(0), stringsAsFactors = FALSE)
res <- summarise(group_by(df, g), n = n())
expect_equal(nrow(res), 0L, info = "summarise() on empty grouped data returns an empty data.frame #1")
expect_equal(colnames(res), c("g", "n"), info = "summarise() on empty grouped data returns an empty data.frame #2")

df <- data.frame(a = seq_len(300), b = rev(seq_len(300)), c = rep(1:3, 100))
expect_equal(
  nrow(summarise(group_by(df, a, b, c), n = n())),
  300L,
  info = "summarise() chooses .groups without tabulating every combination of the groups"
)

gf <- data.frame(g = c("x", "y", "x"), v = 1:3, stringsAsFactors = FALSE) %>% group_by(g)
expect_equal(
  class(summarise(gf, k = list(cur_group()))),
  "data.frame",
  info = "summarise() drops the last group with one grouping variable: list results"
)
gf <- data.frame(f = factor(c("a", "b", "a"), levels = c("a", "b", "c")), v = 1:3) %>% group_by(f, .drop = FALSE)
expect_equal(
  class(summarise(gf, n = n())),
  "data.frame",
  info = "summarise() drops the last group with one grouping variable: .drop = FALSE"
)
gf <- data.frame(g = character(0), v = integer(0), stringsAsFactors = FALSE) %>% group_by(g)
expect_equal(
  class(summarise(gf, n = n())),
  "data.frame",
  info = "summarise() drops the last group with one grouping variable: zero rows"
)

df <- data.frame(g = c("x", "y", "x"), v = 1:3, stringsAsFactors = FALSE)
expect_equal(
  summarise(group_by(df, g), a = NULL, s = sum(v)),
  data.frame(g = c("x", "y"), s = c(4L, 2L), stringsAsFactors = FALSE),
  info = "summarise() ignores NULL results: grouped"
)
expect_equal(
  summarise(df, a = NULL, s = sum(v)),
  data.frame(s = 6L),
  info = "summarise() ignores NULL results: ungrouped"
)
expect_equal(
  summarise(df, s = sum(v), a = if (FALSE) 1),
  data.frame(s = 6L),
  info = "summarise() ignores NULL results: last expression"
)


# Evaluation environment ------------------------------------------------------

f <- function() {
  k <- 10
  list(
    ungrouped = summarise(mtcars, a = mean(mpg) * k),
    grouped = summarise(group_by(mtcars, am), a = mean(mpg) * k)
  )
}
res <- f()
expect_equal(
  res$ungrouped,
  data.frame(a = mean(mtcars$mpg) * 10),
  info = "summarise() finds local variables: ungrouped"
)
expect_equal(
  res$grouped,
  data.frame(am = c(0, 1), a = unname(tapply(mtcars$mpg, mtcars$am, mean)) * 10),
  info = "summarise() finds local variables: grouped"
)

f <- function() {
  i <- 100
  fns <- 7
  group <- 3
  context <- 5
  summarise(group_by(mtcars, am), i = i, fns = fns, group = group, context = context)
}
expect_equal(
  f(),
  data.frame(am = c(0, 1), i = 100, fns = 7, group = 3, context = 5),
  info = "summarise() variables are not masked by poorman's own variables"
)

expect_equal(
  summarise(group_by(mtcars[, c("am", "mpg")], am), a = mean(.data$mpg), .groups = "drop"),
  data.frame(am = c(0, 1), a = unname(tapply(mtcars$mpg, mtcars$am, mean))),
  info = "summarise() supports the .data pronoun in each group"
)

expect_equal(
  summarise(
    group_by(mtcars, am),
    a = nrow(summarise(cur_data(), m = mean(mpg))),
    b = n()
  ),
  data.frame(am = c(0, 1), a = 1L, b = c(19L, 13L)),
  info = "summarise() can be used inside summarise()"
)

# Names -----------------------------------------------------------------------

expect_equal(
  names(summarise(mtcars, a = mean(mpg), sum(disp))),
  c("a", "sum(disp)"),
  info = "summarise() names unnamed expressions among named ones"
)
expect_equal(
  names(summarise(mtcars, mean(mpg + disp + hp + drat + wt + qsec + vs + am + gear + carb + mpg + disp + hp))),
  "mean(...)",
  info = "summarise() abbreviates the names of long expressions, as rlang::as_label()"
)
expect_equal(
  names(summarise(mtcars, .data$mpg[1L])),
  ".data$mpg[1L]",
  info = "summarise() names expressions which use the .data pronoun"
)
expect_equal(
  summarise(mtcars, q = quantile(mpg, c(0.25, 0.75)))$q,
  quantile(mtcars$mpg, c(0.25, 0.75)),
  info = "summarise() keeps the name of named vector results: ungrouped"
)
expect_equal(
  summarise(group_by(mtcars, am), q = quantile(mpg, 0.5)),
  data.frame(am = c(0, 1), q = unname(tapply(mtcars$mpg, mtcars$am, median))),
  info = "summarise() keeps the name of named vector results: grouped"
)
expect_equal(
  summarise(data.frame(x = 1), a = 1, a = 2),
  data.frame(a = 2),
  info = "summarise() keeps the last of duplicated names"
)
expect_equal(
  summarise(data.frame(x = 1), a = 1, b = 2, a = NULL),
  data.frame(a = 1, b = 2),
  info = "summarise() ignores a NULL result with the name of an earlier result"
)
expect_equal(
  summarise(mtcars, list(a = 1, b = 2))[["list(a = 1, b = 2)"]],
  list(a = 1, b = 2),
  info = "summarise() names unnamed list results after the expression"
)
expect_identical(
  names(summarise(mtcars, mean(.data$mpg), sum(wt))),
  c("mean(.data$mpg)", "sum(wt)"),
  info = "summarise() names unnamed expressions which use the .data pronoun"
)
expect_identical(
  names(summarise(mtcars, min(mpg) + max(mpg) + sum(mpg) + mean(disp) + median(hp) + sd(drat) + var(qsec))),
  "... + var(qsec)",
  info = "summarise() abbreviates the names of long operator calls, keeping the operator"
)

# Earlier results -------------------------------------------------------------

expect_equal(
  summarise(data.frame(x = 1:4), x = mean(x), y = x + 1),
  data.frame(x = 2.5, y = 3.5),
  info = "summarise() uses an overwritten column in later expressions: ungrouped"
)
expect_equal(
  summarise(group_by(data.frame(g = c(1, 1, 2), x = 1:3), g), x = sum(x), y = x * 10),
  data.frame(g = c(1, 2), x = 3L, y = 30),
  info = "summarise() uses an overwritten column in later expressions: grouped"
)
expect_equal(
  summarise(group_by(mtcars, am), data.frame(a = mean(mpg)), b = a * 2),
  data.frame(
    am = c(0, 1),
    a = unname(tapply(mtcars$mpg, mtcars$am, mean)),
    b = unname(tapply(mtcars$mpg, mtcars$am, mean)) * 2
  ),
  info = "summarise() makes the columns of unnamed data frame results available to later expressions"
)

# Result sizes ----------------------------------------------------------------

df <- data.frame(g = c(1, 1, 2), x = c(1, 2, 10))
expect_equal(
  summarise(group_by(df, g), y = x[x > 5], .groups = "drop"),
  data.frame(g = 2, y = 10),
  info = "summarise() returns no rows for groups with zero length results"
)
expect_equal(
  summarise(group_by(df, g), y = x[x > 50], .groups = "drop"),
  data.frame(g = numeric(0), y = numeric(0)),
  info = "summarise() returns no rows when every group has a zero length result"
)
res <- summarise(group_by(df, g), y = x[x > 5])
expect_equal(group_vars(res), "g", info = "summarise() keeps the groups when a group has a zero length result")
expect_equal(
  summarise(group_by(df, g), y = x[x > 5], n = n(), .groups = "drop"),
  data.frame(g = 2, y = 10, n = 1L),
  info = "summarise() recycles length one results to zero length"
)
expect_error(
  summarise(mtcars, a = 1:2, b = 1:3),
  "`b` must be size 2 or 1, not 3",
  info = "summarise() gives an error for results of incompatible sizes: ungrouped"
)
expect_error(
  summarise(group_by(mtcars, cyl), a = 1:2, b = 1:3),
  "`b` must be size 2 or 1, not 3",
  info = "summarise() gives an error for results of incompatible sizes: grouped"
)
expect_error(
  summarise(group_by(mtcars, cyl), a = if (cyl[1L] == 4) NULL else 1),
  "can't combine NULL and non NULL results",
  info = "summarise() gives an error for NULL results in only some groups"
)

res <- summarise(group_by(mtcars[0, ], cyl, am), n = n())
expect_equal(
  group_vars(res),
  "cyl",
  info = "summarise() drops the last group of zero row data, as every group has one row"
)

# Result types ----------------------------------------------------------------

res <- summarise(group_by(mtcars, am), d = data.frame(a = mean(mpg), b = 1), .groups = "drop")
expect_equal(names(res), c("am", "d"), info = "summarise() keeps named data frame results as data frame columns #1")
expect_equal(
  res$d,
  data.frame(a = unname(tapply(mtcars$mpg, mtcars$am, mean)), b = 1),
  info = "summarise() keeps named data frame results as data frame columns #2"
)
res <- summarise(group_by(mtcars, am), a = across(c(mpg, disp), mean), .groups = "drop")
expect_equal(names(res), c("am", "a"), info = "summarise() keeps named across() results as data frame columns")
res <- summarise(group_by(mtcars, am), m = matrix(1:2, 1L), .groups = "drop")
expect_equal(names(res), c("am", "m"), info = "summarise() keeps matrix results as matrix columns #1")
expect_equal(res$m, matrix(c(1L, 1L, 2L, 2L), 2L), info = "summarise() keeps matrix results as matrix columns #2")
res <- summarise(group_by(mtcars, am), d = as.Date("2020-01-01") + n(), f = factor(am[1L]), .groups = "drop")
expect_equal(res$d, as.Date("2020-01-01") + c(19, 13), info = "summarise() combines Date results")
expect_equal(res$f, factor(c(0, 1)), info = "summarise() combines factor results")

res <- summarise(group_by(mtcars, cyl), cyl = cyl + 1)
expect_equal(names(res), "cyl", info = "summarise() replaces a grouping column with a result of the same name #1")
expect_equal(
  res$cyl,
  sort(mtcars$cyl) + 1,
  info = "summarise() replaces a grouping column with a result of the same name #2"
)

gf <- group_by(data.frame(f = factor("a", levels = c("a", "b")), x = 1L), f, .drop = FALSE)
expect_equal(
  summarise(gf, k = cur_group()$f)$k,
  factor(c("a", "b")),
  info = "cur_group() gives the keys of empty groups"
)

rm(df, gf, res)

ce_df <- data.frame(x = 1:3, y = c(2, 4, 6))
ce_nm <- "tot"
expect_equal(
  summarise(ce_df, z := mean(y), "{ce_nm}_sum" := sum(y), "{toupper(ce_nm)}" := z * 2), # nolint: object_name_linter.
  data.frame(z = 4, tot_sum = 12, TOT = 8),
  info = "summarise() names := arguments after a symbol or a gluestick template"
)
expect_equal(
  names(summarise(ce_df, `{ce_nm}` := mean(y))), # nolint: object_name_linter.
  "{ce_nm}",
  info = "A symbol on the left of := is not interpolated"
)
ce_fn <- function(data) {
  local_nm <- "inner"
  summarise(data, "{local_nm}" := mean(y)) # nolint: object_name_linter.
}
expect_equal(names(ce_fn(ce_df)), "inner", info = "Gluestick templates are filled in from the calling environment")
expect_error(
  summarise(ce_df, z = w := y),
  "Can't supply both `=` and `:=`",
  info = "An argument can't be named with both = and :="
)
expect_error(
  summarise(ce_df, f(z) := y),
  "The LHS of `:=` must be a string or a symbol.",
  info = "The left of := must be a string or a symbol"
)

ds_df <- data.frame(x = 1:3, y = c(2, 4, 6))
ds_var <- "y"
expect_equal(
  names(summarise(ds_df, .data[[ds_var]], mean(.data[[ds_var]]), .data[[paste0("", ds_var)]] + .data$x)),
  c("y", "mean(.data[[\"y\"]])", ".data[[\"y\"]] + .data$x"),
  info = "summarise() names .data[[var]] after the value of var"
)
ds_fn <- function(data) {
  inner <- "x"
  names(summarise(data, .data[[inner]], max(.data[[inner]])))
}
expect_equal(
  ds_fn(ds_df), c("x", "max(.data[[\"x\"]])"),
  info = "The subscript of .data[[var]] is found in the calling environment"
)
