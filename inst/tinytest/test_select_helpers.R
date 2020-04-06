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

expect_equal(
  mtcars %>% select(contains("a")),
  mtcars[, c("drat", "am", "gear", "carb")],
  info = "Test select() with contains() and a single variable"
)

expect_equal(
  mtcars %>% select(contains(c("a", "m"))),
  mtcars[, c("drat", "am", "gear", "carb", "mpg")],
  info = "Test select() with contains() and multiple variables"
)

expect_equal(
  mtcars %>% select(contains("M", ignore.case = FALSE)),
  mtcars[, 0],
  info = "Test select() with contains() and no match"
)

expect_equal(
  mtcars %>% select(last_col()),
  mtcars[, "carb", drop = FALSE],
  info = "Test select() with last_col()"
)

expect_equal(
  mtcars %>% select(last_col(2)),
  mtcars[, "am", drop = FALSE],
  info = "Test select() with last_col() and an offset"
)

expect_error(
  mtcars %>% select(last_col(1.2)),
  info = "Test last_col() returns an error when not given a whole number"
)
