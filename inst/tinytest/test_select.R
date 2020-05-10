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

expect_equal(
  mtcars %>% select(MilesPerGallon = "mpg", NumberOfGears = "gear"),
  {
    res <- mtcars[, c("mpg", "gear")]
    colnames(res) <- c("MilesPerGallon", "NumberOfGears")
    res
  },
  info = "Test renaming columns when selecting using characters"
)

expect_equal(
  mtcars %>% select(MilesPerGallon = 1, Horsepower = "hp", NumberOfGears = gear),
  {
    res <- mtcars[, c("mpg", "hp", "gear")]
    colnames(res) <- c("MilesPerGallon", "Horsepower", "NumberOfGears")
    res
  },
  info = "Test renaming columns when selecting using a mixture of integer, character and nse names"
)

expect_equal(
  mtcars %>% select(11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1),
  mtcars[, rev(colnames(mtcars))],
  info = "Test order of selection is preserved"
)

expect_equal(
  mtcars %>% select(1, 1, 1, 1),
  mtcars[, "mpg", drop = FALSE],
  info = "Test selecting the same column multiple times only returns it once"
)
