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
  data.frame(),
  info = "empty summarise returns empty data.frame"
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

rm(df, gf, res)
