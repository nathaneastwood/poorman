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
  iris %>% select(matches(".t.")),
  iris[, 1:4],
  info = "Test matches() selects columns based on a regular expression"
)

expect_equal(
  mtcars %>% select(all_of(c("disp", "qsec"))),
  mtcars[, c("disp", "qsec")],
  info = "Test `select()` with `all_of()` and valid columns"
)

expect_error(
  mtcars %>% relocate(all_of(c("disp", "gear", "tmp"))),
  info = "Test errors are returned when `all_of()` has a single incorrect column"
)

expect_error(
  mtcars %>% relocate(all_of(c("disp", "gear", "tmp", "blah"))),
  info = "Test errors are returned when `all_of()` has a multiple incorrect columns"
)

expect_equal(
  mtcars %>% relocate(any_of(c("disp", "gear", "tmp", "cyl")), .before = mpg),
  {
    cols <- c("cyl", "disp", "gear")
    mtcars[, c(cols, colnames(mtcars)[which(!colnames(mtcars) %in% cols)])]
  },
  info = "Test that `any_of()` selects columns regardless of their validity"
)

expect_equal(
  data.frame("V1" = 1, "V2" = 2, "V3" = 3, "V4" = 4) %>% select(num_range("V", 2:3)),
  data.frame("V2" = 2, "V3" = 3),
  info = "Test num_range() within a selection"
)

expect_equal(
  data.frame("V001" = 1, "V002" = 2, "V003" = 3, "V004" = 4) %>% select(num_range("V", 2:3, width = 3)),
  data.frame("V002" = 2, "V003" = 3),
  info = "Test that num_range() works for a given width"
)

expect_error(
  data.frame("a1" = 1, "a1" = 2, "a2" = 3, check.names = FALSE) %>% select(num_range("a", 1)),
  info = "Test that num_range() only works when column names are unique"
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

expect_error(
  mtcars %>% select(last_col(100)),
  info = "Test an error occurs when the user attempts to select using `last_col()` with too large an offset"
)

expect_error(
  mtcars %>% select(last_col(vars = c())),
  info = "Test an error occurs when no variables are provided to match column names against"
)

expect_equal(
  mtcars %>% select(everything()),
  mtcars,
  info = "Test everything() returns all columns in the same order"
)

expect_equal(
  mtcars %>% select(starts_with("q"), ends_with("s"), everything()),
  mtcars[, c("qsec", "vs", colnames(mtcars)[!colnames(mtcars) %in% c("qsec", "vs")])],
  info = "Test everything() returns the rest of the columns after the initial selection"
)
