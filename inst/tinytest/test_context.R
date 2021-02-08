df <- data.frame(
  g = c("a", "b", "b", "c", "c", "c"),
  x = c(1, 4, 5, 7, 3, 6),
  y = c(23, 46, 7, 4, 56, 12),
  stringsAsFactors = FALSE
)
gf <- df %>% group_by(g)

expect_equal(
  df %>% summarise(n()),
  structure(list(`n()` = 6L), class = "data.frame", row.names = c(NA, -1L)),
  info = "n() correctly counts the number of rows"
)

expect_equal(
  gf %>% summarise(n = n()) %>% ungroup(),
  structure(list(g = c("a", "b", "c"), n = 1:3), row.names = c(NA, -3L), class = "data.frame"),
  info = "n() works within a grouped context"
)

expect_equal(
  {
    res <- gf %>% mutate(id = cur_group_id()) %>% ungroup()
    res[, "id"]
  },
  c(1L, 2L, 2L, 3L, 3L, 3L),
  info = "cur_group_id() works as expected"
)

expect_equal(
  {
    res <- gf %>% summarise(row = cur_group_rows())
    res[, "row"]
  },
  1L:6L,
  info = "cur_group_rows() works as expected"
)

expect_equal(
  {
    res <- gf %>% summarise(data = list(cur_group()))
    res[, "data"]
  },
  list(
    structure(list(g = "a"), row.names = c(NA, -1L), class = "data.frame"),
    structure(list(g = "b"), row.names = c(NA, -1L), class = "data.frame"),
    structure(list(g = "c"), row.names = c(NA, -1L), class = "data.frame")
  ),
  info = "cur_group() works as expected"
)

expect_equal(
  {
    res <- gf %>% summarise(data = list(cur_data()))
    res[, "data"]
  },
  list(
    structure(list(x = 1, y = 23), class = "data.frame", row.names = 1L),
    structure(list(x = c(4, 5), y = c(46, 7)), class = "data.frame", row.names = 2:3),
    structure(list(x = c(7, 3, 6), y = c(4, 56, 12)), class = "data.frame", row.names = 4:6)
  ),
  info = "cur_data() works as expected"
)

# errors -----------------------------------------------------------------------
expect_error(
  mtcars %>% select(n()),
  info = "`n()` cannot be used outside of the context"
)

expect_error(
  mtcars %>% select(cur_data()),
  info = "`cur_data()` cannot be used outside of the context"
)

expect_error(
  mtcars %>% select(cur_group()),
  info = "`cur_group()` cannot be used outside of the context"
)

expect_error(
  mtcars %>% select(cur_group_id()),
  info = "`cur_group_id()` cannot be used outside of the context"
)

expect_error(
  mtcars %>% select(cur_group_rows()),
  info = "`cur_group_rows()` cannot be used outside of the context"
)
