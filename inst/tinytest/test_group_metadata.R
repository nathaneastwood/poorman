# group_data() -----------------------------------------------------------------

res <- data.frame(x = 1:3) %>% group_data()
expect_identical(class(res), "data.frame", info = "group_data() returns a data frame")
expect_equal(
  res,
  structure(list(.rows = list(1:3)), class = "data.frame", row.names = c(NA, -1L)),
  info = "group_data() returns all rows from .data"
)

res <- data.frame(x = c(1, 1, 2)) %>% group_by(x) %>% group_data()
expect_identical(class(res), "data.frame")
expect_equivalent(
  res,
  structure(list(x = c(1, 2), .rows = list(1:2, 3L)), row.names = c(NA, -2L), class = "data.frame"),
  info = "group_data() returns the unique rows for each group"
)

# group_rows() -----------------------------------------------------------------

df <- data.frame(x = 1:2, y = 1:2) %>% group_by(x, y) %>% group_data()
gf <- group_by(df, x, y)
gd <- group_data(gf)
expect_equal(group_rows(gf), gd[[".rows"]], info = "group_rows() partitions group_data()")

# group_indices() ---------------------------------------------------------

df <- data.frame(x = c("b", "a", "b"), stringsAsFactors = FALSE)
gf <- group_by(df, x)
expect_equal(group_indices(df), c(1, 1, 1), info = "group_indices() returns expected values for ungrouped data")
expect_equal(group_indices(gf), c(2, 1, 2), info = "group_indices() returns expected values for grouped data")

# group_size --------------------------------------------------------------

df <- data.frame(x = rep(1:3, each = 10), y = rep(1:6, each = 5))
expect_equal(n_groups(df), 1L, info = "Ungrouped data has 1 group")
expect_equal(group_size(df), 30, info = "Ungrouped data has group_size() equal to nrow(data)")

df <- data.frame(x = rep(1:3, each = 10), y = rep(1:6, each = 5)) %>% group_by(x)
expect_equal(n_groups(df), 3L, info = "n_groups() is correct for grouped data")
expect_equal(group_size(df), rep(10, 3), info = "group_size() is correct for grouped data")

# groups -----------------------------------------------------------------------
expect_equal(
  df %>% groups(),
  list(as.symbol("x")),
  info = "groups() returns list of group variables as symbols"
)

rm(res, df, gd, gf)

df <- data.frame(f = factor(c("c", "a"), levels = c("a", "b", "c")))
expect_equal(
  group_indices(group_by(df, f, .drop = FALSE)),
  c(3L, 1L),
  info = "group_indices() counts empty groups"
)
df <- data.frame(g = c("b", NA, "a", NA), stringsAsFactors = FALSE)
expect_equal(group_indices(group_by(df, g)), c(2L, 3L, 1L, 3L), info = "group_indices() handles missing values")

df <- data.frame(g = c(2, 1, 2), x = 1:3) %>% group_by(g)
expect_equal(group_indices(df[c(2, 1, 3), ]), c(1L, 2L, 2L), info = "group_indices() recalculates out of date groups")

rm(df)
