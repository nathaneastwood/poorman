expect_equal(
  coalesce(c(1, NA, 3, NA, 5), 0L),
  c(1, 0, 3, 0, 5),
  info = "Use a single value to replace all missing vectors"
)

expect_equal(
  {
    y <- c(1, 2, NA, NA, 5)
    z <- c(NA, NA, 3, 4, 5)
    coalesce(y, z)
  },
  1:5,
  info = "Match together a complete vector from missing pieces"
)

expect_equal(
  coalesce(NA, 2, c(1, 2, 3)),
  rep(2, 3),
  info = "Multiple vectors work"
)

expect_error(
  coalesce(c(1, NA, NA), c(1, 2)),
  info = "Differing lengths are not allowed when at least one vector is length 1: length(..1) > length(..2)"
)
expect_error(
  coalesce(c(1, NA), c(1, 2, 3)),
  info = "Differing lengths are not allowed when at least one vector is length 1: length(..1) < length(..2)"
)
expect_error(
  coalesce(NA, c(2, 3), c(1, 2, 3)),
  info = "Multiple vector lengths fails"
)
