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
