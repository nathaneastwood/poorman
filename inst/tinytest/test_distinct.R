df <- data.frame(x = c(1, 1, 1, 1), y = c(1, 1, 2, 2), z = c(1, 2, 1, 2))
expect_equal(
  distinct(df),
  df,
  info = "distinct() is equivalent to unique.data.frame() when .keep_all is FALSE"
)

df <- data.frame(x = c(1, 1, 1, 1), y = c(1, 1, 2, 2), z = c(1, 2, 1, 2))
expect_equal(
  distinct(df, x, .keep_all = FALSE),
  unique(df["x"]),
  info = "distinct() for single column works as expected"
)
expect_equal(
  distinct(df, y, .keep_all = FALSE),
  data.frame(y = 1:2),
  info = "distinct() for single column works as expected"
)

expect_equal(
  ncol(distinct(data.frame())),
  0L,
  info = "distinct() works for 0-column data.frames"
)

expect_equal(
  distinct(data.frame(x = c(1, 1), y = c(2, 2))),
  data.frame(x = 1, y = 2),
  info = "If no variables are specified, test distinct() uses all columns"
)

expect_equal(
  data.frame(x = c(1, 1, 1), y = 3:1) %>% distinct(x),
  data.frame(x = 1),
  info = "distinct() keeps only specified columns"
)

expect_equal(
  data.frame(x = c(1, 1, 1), y = 3:1) %>% distinct(x, .keep_all = TRUE),
  data.frame(x = 1, y = 3L),
  info = "distinct() keeps all columns when .keep_all = TRUE"
)

expect_equal(
  data.frame(a = 1:3, b = 4:6) %>% distinct(a, a) %>% colnames(),
  "a",
  "distinct() doesn't duplicate columns"
)

expect_equal(
  row.names(distinct(df, y)),
  c("1", "2"),
  info = "distinct() reset row names when they are numeric"
)

expect_equal(
  row.names(distinct(head(mtcars, 3), mpg)),
  c("Mazda RX4", "Datsun 710"),
  info = "distinct() keeps row names when they are character"
)

expect_equal(
  data.frame(a = 1:3, b = 4:6) %>% group_by(a) %>% distinct(a) %>% colnames(),
  "a",
  info = "Grouped distinct() doesn't duplicate columns"
)

expect_equal(
  data.frame(g = c(1, 2), x = c(1, 2)) %>% group_by(g) %>% distinct(x) %>% colnames(),
  c("g", "x"),
  info = "Grouped distinct() always includes group columns"
)

df <- data.frame(g = c(1, 2), x = c(1, 2))
expect_equal(
  df %>% distinct() %>% group_by(g),
  df %>% group_by(g) %>% distinct(),
  info = "Empty grouped distinct() equivalent to empty ungrouped"
)

df <- data.frame(g = c(1, 2), x = c(1, 2))
expect_equal(
  df %>% distinct(aa = g * 2),
  df %>% mutate(aa = g * 2) %>% distinct(aa),
  info = "distinct() on a new, mutated variable is equivalent to mutate() followed by distinct()"
)

df <- data.frame(g = c(1, 2), x = c(1, 2))
expect_equal(
  df %>% distinct(aa = g),
  df %>% mutate(aa = g) %>% distinct(aa),
  info = "distinct() on a new, copied variable is equivalent to mutate() followed by distinct()"
)

expect_equal(
  data.frame(a = c(1, 2), b = c(1, 2)) %>% distinct(a, b * 2),
  data.frame(a = c(1, 2), `b * 2` = c(2, 4), check.names = FALSE),
  info = "Non-named mutations arguments are returned"
)

expect_equal(
  data.frame(a = c(1, 2), b = c(1, 2)) %>% distinct(a, b2 = b * 2),
  data.frame(a = c(1, 2), b2 = c(2, 4), check.names = FALSE),
  info = "Named mutations arguments are returned"
)

expect_equal(
  structure(list(), .Names = character(0), row.names = c(NA, -2L), class = "data.frame") %>%
    distinct() %>%
    nrow(),
  1L,
  info = "distinct() handles 0 columns edge case"
)

expect_equal(
  names(distinct(data.frame(x = 1:2, y = 3:4), y, x)),
  c("x", "y"),
  info = "distinct() preserves order of the input variables"
)

gf <- data.frame(x = c(1, 1, 2, 2), y = c(1, 1, 2, 2)) %>% group_by(x)
expect_equal(
  gf %>% distinct() %>% group_vars(),
  "x",
  info = "distinct() preserves grouping"
)

expect_equal(
  group_vars(distinct(gf, x = x + 2)),
  "x",
  info = "Grouped distinct() preserves grouping"
)

rm(df, gf)
