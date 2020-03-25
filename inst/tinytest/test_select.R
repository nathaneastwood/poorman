expect_equal(
  mtcars %>% select(mpg),
  mtcars[, "mpg", drop = FALSE],
  info = "Test selecting a single column"
)

expect_equal(
  mtcars %>% select(1),
  mtcars[, "mpg", drop = FALSE],
  info = "Test selecting a single integer column"
)

expect_equal(
  mtcars %>% select(mpg, cyl, disp),
  mtcars[, c("mpg", "cyl", "disp")],
  info = "Test selecting multiple columns"
)

expect_equal(
  mtcars %>% select(1, 2, 3),
  mtcars[, c("mpg", "cyl", "disp")],
  info = "Test selecting multiple integer columns"
)

expect_equal(
  mtcars %>% select(mpg, 2, disp, 4, 5, gear),
  mtcars[, c("mpg", "cyl", "disp", "hp", "drat", "gear")],
  info = "Test mixed integer and nse column names"
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

expect_equal(
  mtcars %>% select(MilesPerGallon = 1, NumberOfGears = 10),
  {
    res <- mtcars[, c("mpg", "gear")]
    colnames(res) <- c("MilesPerGallon", "NumberOfGears")
    res
  },
  info = "Test renaming columns when selecting using integers"
)
