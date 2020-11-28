batman <- c(NA, NA, NA, NA, NA)
expect_true(all(is.na(cumany(batman))), info = "cumany() handles all NA vectors")
expect_true(all(is.na(cumall(batman))), info = "cumall() handles all NA vectors")

expect_identical(
  cumall(c(TRUE, NA, FALSE, NA)),
  c(TRUE, NA, FALSE, FALSE),
  info = "cumall: handles NAs #1"
)

expect_identical(
  cumall(c(FALSE, NA, TRUE)),
  c(FALSE, FALSE, FALSE),
  info = "cumall: handles NAs #2"
)

expect_identical(
  cumall(c(NA, TRUE)),
  c(NA, NA),
  info = "cumall: handles NAs #3"
)

expect_identical(
  cumall(c(NA, FALSE)),
  c(NA, FALSE),
  info = "cumall: handles NAs #4"
)

expect_identical(
  cumany(c(TRUE, NA, FALSE)),
  c(TRUE, TRUE, TRUE),
  info = "cumany: handles NAs #1"
)

expect_identical(
  cumany(c(FALSE, NA, TRUE)),
  c(FALSE, NA, TRUE),
  info = "cumall: handles NAs #2"
)

# scalars
expect_true(is.na(cumall(NA)), info = "cumall: NA")
expect_true(is.na(cumany(NA)), info = "cumany: NA")
expect_true(cumall(TRUE), info = "cumall: TRUE")
expect_false(cumall(FALSE), info = "cumall: FALSE")
expect_true(cumany(TRUE), info = "cumany: TRUE")
expect_false(cumany(FALSE), info = "cumany: FALSE")

# degenerate cases
expect_identical(cumall(logical()), logical(), info = "cumall: empty input returns logical(0)")
expect_identical(cumany(logical()), logical(), info = "cumany: empty input returns logical(0)")

x <- as.raw(c(2L, 9L, 0L))
class(x) <- "logical"
expect_identical(
  cumall(x),
  x == TRUE,
  "cumall: behaviour of degenerate logical vectors mimics that of base R functions"
)
expect_identical(
  cumany(x),
  c(TRUE, TRUE, TRUE),
  "cumany: behaviour of degenerate logical vectors mimics that of base R functions"
)

x <- 1:5
expect_equal(cummean(x), c(1, 1.5, 2, 2.5, 3), info = "cummean returns expected output")
expect_equal(cummean(x), cumsum(x) / seq_along(x), info = "cummean is consistent with cumsum() and seq_along()")
x <- c(1, 3, NA, 5, 2, 2)
expect_equal(cummean(x), c(1, 2, NA, NA, NA, NA), info = "cummean handles NAs correctly")
expect_equal(cummean(numeric()), numeric(), info = "empty input returns numeric(0)")
