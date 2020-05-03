df1 <- data.frame(
  id = c(1, 3, 5, 7, 9),
  name = LETTERS[1:5],
  height = c(1, 2, 2, 2, 2),
  stringsAsFactors = FALSE
)

df2 <- data.frame(
  id = c(2, 4, 5, 7),
  name = LETTERS[1:4],
  weight = c(2, 3, 4, 5),
  stringsAsFactors = FALSE
)

expect_equal(
  {
    invisible(suppressMessages(res <- df1 %>% inner_join(df2)))
    res
  },
  data.frame(
    id = c(5, 7),
    name = LETTERS[3:4],
    height = c(2, 2),
    weight = c(4, 5),
    stringsAsFactors = FALSE
  ),
  info = "Inner join"
)

expect_equal(
  {
    invisible(suppressMessages(res <- df1 %>% left_join(df2)))
    res
  },
  data.frame(
    id = c(1, 3, 5, 7, 9),
    name = LETTERS[1:5],
    height = c(1, 2, 2, 2, 2),
    weight = c(NA, NA, 4, 5, NA),
    stringsAsFactors = FALSE
  ),
  info = "Left join"
)

expect_equal(
  {
    invisible(suppressMessages(res <- df1 %>% right_join(df2)))
    res
  },
  data.frame(
    id = c(5, 7, 2, 4),
    name = LETTERS[c(3, 4, 1, 2)],
    height = c(2, 2, NA, NA),
    weight = c(4, 5, 2, 3),
    stringsAsFactors = FALSE
  ),
  info = "Right join"
)

expect_equal(
  {
    invisible(suppressMessages(res <- df1 %>% full_join(df2)))
    res
  },
  data.frame(
    id = c(1, 3, 5, 7, 9, 2, 4),
    name = c("A", "B", "C", "D", "E", "A", "B"),
    height = c(1, rep(2, 4), NA, NA),
    weight = c(NA, NA, 4, 5, NA, 2, 3),
    stringsAsFactors = FALSE
  ),
  info = "Full join"
)

df1 <- data.frame(
  a = c(1, 2, 3, 4, 5, 6),
  b = c("a", "b", "f", "e", "r", "h"),
  d = c("q", "l", "o", "n", "q", "z"),
  stringsAsFactors = FALSE
)

df2 <- data.frame(
  a = c(1, 2, 3, 4, 5, 6),
  d = c("q", "l", "o", "n", "q", "z"),
  stringsAsFactors = FALSE
)

expect_equal(
  df1 %>% left_join(df2, by = c("a", "d")),
  data.frame(
    a = c(1, 2, 3, 4, 5, 6),
    d = c("q", "l", "o", "n", "q", "z"),
    b = c("a", "b", "f", "e", "r", "h"),
    stringsAsFactors = FALSE
  ),
  info = "Explicit `by` parameter"
)

expect_equal(
  df1 %>% left_join(df2, by = "a", suffix = c("_x", "_y")) %>% colnames(),
  c("a", "b", "d_x", "d_y"),
  info = "Suffix changes"
)

df1 <- data.frame(
  a1 = c(1, 2, 3, 4, 5, 6),
  b = c("a", "b", "f", "e", "r", "h"),
  d = c("q", "l", "o", "n", "q", "z"),
  stringsAsFactors = FALSE
)

df2 <- data.frame(
  a2 = c(1, 2, 3, 4, 5, 6),
  d = c("q", "l", "o", "n", "q", "z"),
  stringsAsFactors = FALSE
)

expect_equal(
  df1 %>% left_join(df2, by = c("a1" = "a2", "d" = "d")),
  data.frame(
    a1 = c(1, 2, 3, 4, 5, 6),
    d = c("q", "l", "o", "n", "q", "z"),
    b = c("a", "b", "f", "e", "r", "h"),
    stringsAsFactors = FALSE
  ),
  info = "Non-matching column names"
)
