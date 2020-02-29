expect_error(
  mtcars %>% pull(mpg, cyl),
  info = "Expect error when pulling multiple columns"
)

expect_equal(
  mtcars %>% pull(mpg),
  mtcars[, "mpg"],
  info = "Test pulling a single column"
)
