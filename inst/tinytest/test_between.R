expect_equal(
  between(1, NA, 1),
  NA,
  info = "Return NA if any argument is NA"
)

x <- runif(1e3)
expect_equal(
  between(x, 0.25, 0.5),
  x >= 0.25 & x <= 0.5,
  info = "Return the expected results"
)
rm(x)

expect_warning(
  between(structure(c(1, 5), class = "foo"), 1, 3),
  "numeric vector with S3 class",
  info = "Expect warning for numeric vectors with a class"
)

expect_warning(
  between(factor("x"), 1, 2),
  info = "Expect a warning for S3 class"
)

expect_silent(between(Sys.Date(), 1, 3), info = "Expect no warning for Date")

expect_silent(between(Sys.time(), 1, 3), info = "Expect no warning for POSIXct")
