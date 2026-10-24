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

expect_equal(
  distinct(data.frame(g = c("a", "a", "b")), z := g),
  data.frame(z = c("a", "b")),
  info = "distinct() supports :="
)

# Grouped distinct() keeps the rows in their original order
df <- data.frame(g = c(2, 1, 2, 1, 2, 1), x = c(1, 1, 1, 2, 3, 2), y = c("p", "q", "r", "s", "t", "u"))
gf <- group_by(df, g)
res <- distinct(gf, x)
expect_equal(
  as.data.frame(ungroup(res)), data.frame(g = c(2, 1, 1, 2), x = c(1, 1, 2, 3)),
  info = "grouped distinct() keeps the first rows in their original order"
)
expect_equal(group_data(res)$.rows, list(2:3, c(1L, 4L)), info = "grouped distinct() recalculates the group rows")
expect_equal(
  as.data.frame(ungroup(distinct(gf, x, .keep_all = TRUE))),
  data.frame(g = c(2, 1, 1, 2), x = c(1, 1, 2, 3), y = c("p", "q", "s", "t")),
  info = "grouped distinct() with .keep_all keeps the original order"
)
expect_equal(
  as.data.frame(ungroup(distinct(gf, z = x * 2, .keep_all = TRUE))),
  data.frame(g = c(2, 1, 1, 2), x = c(1, 1, 2, 3), y = c("p", "q", "s", "t"), z = c(2, 2, 4, 6)),
  info = "grouped distinct() with .keep_all puts new columns last"
)
expect_equal(
  as.data.frame(ungroup(distinct(gf, m = mean(x)))), data.frame(g = c(2, 1), m = c(5, 5) / 3),
  info = "grouped distinct() evaluates expressions for each group"
)
expect_equal(
  as.data.frame(ungroup(distinct(gf, g = g * 10))), data.frame(g = c(20, 10)),
  info = "grouped distinct() can change the grouping column"
)
f <- function() {
  k <- 10
  distinct(gf, z = x + k)
}
expect_equal(f()$z, c(11, 11, 12, 13), info = "grouped distinct() finds local variables of the caller")
res <- distinct(group_by(data.frame(g = c(NA, 1, NA), x = c(1, 1, 1)), g))
expect_equal(res$g, c(NA, 1), info = "grouped distinct() keeps missing groups in their original order")
fd <- group_by(data.frame(g = factor(c("x", "x"), levels = c("x", "y")), v = c(1, 1)), g, .drop = FALSE)
expect_equal(
  group_data(distinct(fd, v))$.rows, list(1L, integer()),
  info = "grouped distinct() keeps empty groups with .drop = FALSE"
)
rm(df, gf, res, f, fd)
