x <- c(0, 1, 0)
expect_equal(
  na_if(x, 0),
  c(NA, 1, NA),
  info = "scalar y replaces all matching x"
)
expect_equal(
  na_if(x, 1),
  c(0, NA, 0),
  info = "scalar y replaces all matching x"
)

expect_error(
  na_if(x, 1:10),
  info = "x and y must be the same length"
)
expect_error(
  na_if(1, 1:2),
  info = "x and y must be the same length"
)
