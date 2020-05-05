expect_equal(
  if_else(c(TRUE, TRUE, FALSE, FALSE), 1, 2),
  c(1, 1, 2, 2),
  info = "Scalar true and false are vectorised"
)

expect_error(
  if_else(c(TRUE, TRUE, FALSE, FALSE), 1, "2"),
  info = "true and false should be the same type"
)

expect_equal(
  {
    x <- c(-1, 0, 1)
    if_else(x < 0, x, 0)
  },
  c(-1, 0, 0),
  info = "Vectorised true works"
)

expect_equal(
  {
    x <- c(-1, 0, 1)
    if_else(x > 0, x, 0)
  },
  c(0, 0, 1),
  info = "Vectorised false works"
)

expect_equal(
  if_else(c(TRUE, NA, FALSE), -1, 1),
  c(-1, NA, 1),
  info = "Missing values are missing by default"
)

expect_equal(
  if_else(c(TRUE, NA, FALSE), -1, 1, 0),
  c(-1, 0, 1),
  info = "Missing values are replaced"
)

expect_error(
  if_else(c(TRUE, NA, FALSE), -1, 1, "should fail"),
  info = "missing should be the same type as true and false"
)

expect_equal(
  {
    x <- factor(letters[1:5])
    if_else(x %in% c("a", "b", "c"), x, factor(NA))
  },
  factor(c("a", "b", "c", NA, NA), levels = letters[1:5]),
  info = "if_else works with factors"
)

expect_equal(
  {
    x <- list(1, 2, 3)
    if_else(c(TRUE, TRUE, FALSE), x, list(NULL))
  },
  list(1, 2, NULL),
  info = "if_else works with lists"
)
