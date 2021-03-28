expect_equal(
  arrange(mtcars),
  mtcars,
  info = "Empty arrange returns the input"
)

gf <- group_by(mtcars, am)
expect_equal(
  arrange(gf),
  gf,
  info = "Empty arrange returns the input"
)

df <- data.frame(a = numeric(0))
expect_equal(
  arrange(df, a),
  df,
  info = "can sort empty data frame"
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

df <- data.frame(x = 1:3, y = 3:1 + 2i)
expect_equal(arrange(df, y), df[3:1, ], info = "arrange handles complex columns")

# Grouped Operations
df <- data.frame(g = c(2, 2, 1, 1), x = c(1, 3, 2, 4))
res <- df %>% group_by(g) %>% arrange(x)
expect_true(
  inherits(res, "grouped_df"),
  info = "arrange keeps the grouping class"
)

expect_equal(
  group_rows(res),
  list(c(2L, 4L), c(1L, 3L)),
  info = "arrange keeps the grouping structure"
)

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
  info = "grouped arrange ignores group_by groups"
)
expect_equal(
  arrange(gf, x, .by_group = TRUE),
  structure(
    list(g = c(1, 1, 2, 2), x = c(1L, 3L, 2L, 4L)),
    groups = structure(list(g = c(1, 2), .rows = list(1:2, 3:4)), row.names = 1:2, class = "data.frame", .drop = TRUE),
    row.names = c(4L, 2L, 3L, 1L), class = c("grouped_df", "data.frame")
  ),
  info = "grouped arrange ignores group, unless requested with .by_group"
)
