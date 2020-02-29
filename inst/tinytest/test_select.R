expect_equal(
  mtcars %>% select(mpg),
  mtcars[, "mpg", drop = FALSE],
  info = "Test selecting a single column"
)

expect_equal(
  mtcars %>% select(mpg, cyl, disp),
  mtcars[, c("mpg", "cyl", "disp")],
  info = "Test selecting multiple columns"
)

expect_equal(
  mtcars %>% select(MilesPerGallon = mpg, NumberOfGears = gear),
  {
    res <- mtcars[, c("mpg", "gear")]
    colnames(res) <- c("MilesPerGallon", "NumberOfGears")
    res
  },
  info = "Test renaming columns when selecting"
)
