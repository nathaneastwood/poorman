expect_equal(
  mtcars %>% select(starts_with("d")),
  mtcars[, c("disp", "drat")],
  info = "Test select() using starts_with() with a single value"
)

expect_equal(
  mtcars %>% select(starts_with(c("d", "c"))),
  mtcars[, c("cyl", "disp", "drat", "carb")],
  info = "Test select() using starts_with() for multiple values"
)

expect_equal(
  mtcars %>% relocate(starts_with("c"), .before = mpg),
  mtcars[, c("cyl", "carb", "mpg", "disp", "hp", "drat", "wt", "qsec", "vs", "am", "gear")],
  info = "Test relocate() with starts_with()"
)

expect_equal(
  mtcars %>% select(ends_with("g")),
  mtcars[, "mpg", drop = FALSE],
  info = "Test select() using ends_with() with a single value"
)

expect_equal(
  mtcars %>% select(ends_with(c("g", "b"))),
  mtcars[, c("mpg", "carb")],
  info = "Test select() using ends_with() with a multiple values"
)

expect_equal(
  mtcars %>% relocate(starts_with("c"), .before = mpg),
  mtcars[, c("cyl", "carb", "mpg", "disp", "hp", "drat", "wt", "qsec", "vs", "am", "gear")],
  info = "Test relocate() with ends_with()"
)

expect_equal(
  mtcars %>% select(starts_with("m"), 2, disp, "gear", ends_with("b")),
  mtcars[, c("mpg", "cyl", "disp", "gear", "carb")],
  info = "Test multiple *_with() functions in combination with other selection methods"
)
