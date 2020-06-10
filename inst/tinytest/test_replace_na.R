x <- c(1, NA)
expect_equal(
  replace_na(x),
  x,
  info = "Empty replacement for `vector` does nothing"
)

expect_equal(
  replace_na(x, 0),
  c(1, 0),
  info = "Missing values are replaced in a `vector`"
)

expect_error(
  replace_na(1, 1:10),
  info = "Replacement length should be 1 for `vector`s"
)

df <- data.frame(x = c(1, NA))
expect_equal(
  replace_na(df),
  df,
  info = "Empty replacement for `data.frame` does nothing"
)

expect_equal(
  replace_na(df, list(x = 0))$x,
  c(1, 0),
  info = "Missing values are replaced in a `data.frame`"
)

expect_equal(
  replace_na(df, list(x = 100, b = 0))$x,
  c(1, 100),
  info = "Don't complain about variables that don't exist"
)

rm(df, x)
