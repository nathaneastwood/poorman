df <- data.frame(x = 1:10, y = 1:10)
gf <- group_by(df, x)
expect_identical(
  arrange(df),
  df,
  info = "empty arrange() returns input for data.frames"
)
expect_identical(
  arrange(gf),
  gf,
  info = "empty arrange() returns input for grouped_df"
)

df <- data.frame(a = numeric(0))
expect_equal(
  arrange(df, a),
  df,
  info = "arrange() can sort empty data frame"
)

df <- data.frame(x = c(2, 1, NA))
expect_equal(
  df %>% arrange(x) %>% pull(),
  c(1, 2, NA),
  info = "local arrange() sorts missing values to the end"
)
expect_equal(
  df %>% arrange(desc(x)) %>% pull(),
  c(2, 1, NA),
  info = "local arrange() sorts missing values to the end"
)

# -- column types --------------------------------------------------------------

df <- data.frame(x = 1:3, y = 3:1 + 2i)
expect_equal(
  arrange(df, y),
  df[3:1, ],
  info = "arrange() handles complex columns"
)

TestS4 <- suppressWarnings(setClass("TestS4", contains = "integer"))
setMethod(
  '[',
  'TestS4',
  function(x, i, ...) TestS4(unclass(x)[i, ...])
)
df <- data.frame(x = 1:3, y = TestS4(3:1))
expect_equal(
  arrange(df, y),
  df[3:1, ],
  info = "arrange handles S4 classes"
)
removeClass("TestS4")

# -- data ----------------------------------------------------------------------

df1 <- data.frame(x = 1:3, y = 3:1)
df2 <- df1 %>% group_by(x)
expect_true(
  inherits(arrange(df1, x), "data.frame"),
  info = "arrange() preserves input class for data.frames"
)
expect_true(
  inherits(arrange(df2, x), "grouped_df"),
  info = "arrange() preserves input class for grouped_dfs"
)

df <- data.frame(g = c(2, 2, 1, 1), x = c(1, 3, 2, 4))
res <- df %>% group_by(g) %>% arrange(x)
expect_true(
  inherits(res, "grouped_df"),
  info = "arrange() updates the grouping structure: 1"
)
expect_equal(
  group_rows(res),
  list(c(2L, 4L), c(1L, 3L)),
  info = "arrange() updates the grouping structure: 2"
)

expect_equal(
  mtcars %>% arrange(mpg),
  mtcars[order(mtcars$mpg), ],
  info = "Test ascending column arrangement"
)

expect_equal(
  mtcars %>% arrange(-mpg),
  mtcars[order(-mtcars$mpg), ],
  info = "Test descending column arrangement"
)

expect_equal(
  mtcars %>% arrange(cyl, mpg),
  mtcars[order(mtcars$cyl, mtcars$mpg), ],
  info = "Test multiple column arrangement"
)

expect_equal(
  mtcars %>% arrange(cyl, -mpg),
  mtcars[order(mtcars$cyl -mtcars$mpg), ],
  info = "Test multiple ascending and descending column arrangement"
)

# data masking
df <- data.frame(x = 1:4, y = 5:8)
expect_equal(
  arrange(df, -x * y),
  structure(list(x = 4:1, y = 8:5), row.names = 4:1, class = "data.frame"),
  info = "arrange() can add and arrange by new columns (#89)"
)

# Grouped Operations
df <- data.frame(g = c(2, 2, 1, 1), x = c(1, 3, 2, 4))
res <- df %>% group_by(g) %>% arrange(x)
expect_true(
  inherits(res, "grouped_df"),
  info = "arrange() keeps the grouping class"
)

expect_equal(
  group_rows(res),
  list(c(2L, 4L), c(1L, 3L)),
  info = "arrange() keeps the grouping structure"
)

# .by_group
df <- data.frame(g = c(2, 1, 2, 1), x = 4:1)
gf <- group_by(df, g)
expect_equal(
  arrange(gf, x),
  structure(
    list(g = c(1, 2, 1, 2), x = 1:4),
    groups = structure(
      list(g = c(1, 2), .rows = list(c(1L, 3L), c(2L, 4L))), row.names = 1:2, class = "data.frame", .drop = TRUE
    ),
    row.names = 4:1,
    class = c("grouped_df", "data.frame")
  ),
  info = "grouped arrange() ignores group_by groups"
)
expect_equal(
  arrange(gf, x, .by_group = TRUE),
  structure(
    list(g = c(1, 1, 2, 2), x = c(1L, 3L, 2L, 4L)),
    groups = structure(list(g = c(1, 2), .rows = list(1:2, 3:4)), row.names = 1:2, class = "data.frame", .drop = TRUE),
    row.names = c(4L, 2L, 3L, 1L), class = c("grouped_df", "data.frame")
  ),
  info = "grouped arrange() ignores group, unless requested with .by_group"
)

# with character columns
df <- data.frame(x = c("a", "b", "a", "b"),
                 y = c("c", "c", "c", "d"),
                 z = c(4, 2, 1, 3))
expect_equal(
  df %>% arrange(-x),
  df[order(df$x, decreasing = TRUE), ]
)
expect_equal(
  df %>% arrange(-x, y),
  data.frame(x = c("b", "b", "a", "a"),
             y = c("c", "d", "c", "c"),
             z = c(2, 3, 4, 1)) %>%
    structure(row.names = c(2L, 4L, 1L, 3L))
)
expect_equal(
  df %>% arrange(-x, y, z),
  data.frame(x = c("b", "b", "a", "a"),
             y = c("c", "d", "c", "c"),
             z = c(2, 3, 1, 4)) %>%
    structure(row.names = c(2L, 4L, 3L, 1L))
)

expect_equal(
  df %>% arrange(desc(x)),
  df[order(df$x, decreasing = TRUE), ]
)
expect_equal(
  df %>% arrange(desc(x), y),
  data.frame(x = c("b", "b", "a", "a"),
             y = c("c", "d", "c", "c"),
             z = c(2, 3, 4, 1)) %>%
    structure(row.names = c(2L, 4L, 1L, 3L))
)
expect_equal(
  df %>% arrange(desc(x), y, z),
  data.frame(x = c("b", "b", "a", "a"),
             y = c("c", "d", "c", "c"),
             z = c(2, 3, 1, 4)) %>%
    structure(row.names = c(2L, 4L, 3L, 1L))
)
