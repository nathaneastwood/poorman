expect_equal(
  mtcars %>% summarise(mean(mpg)),
  data.frame("mean(mpg)" = mean(mtcars$mpg), check.names = FALSE),
  info = "Test single summaries"
)

expect_equal(
  mtcars %>% summarise(meanMpg = mean(mpg)),
  data.frame("meanMpg" = mean(mtcars$mpg)),
  info = "Test single summaries with column name"
)

expect_equal(
  mtcars %>% summarise(mean(mpg), sum(disp)),
  data.frame("mean(mpg)" = mean(mtcars$mpg), "sum(disp)" = sum(mtcars$disp), check.names = FALSE),
  info = "Test multiple summarise"
)

expect_equal(
  mtcars %>% summarise(meanMpg = mean(mpg), sumDisp = sum(disp)),
  data.frame("meanMpg" = mean(mtcars$mpg), "sumDisp" = sum(mtcars$disp), check.names = FALSE),
  info = "Test multiple summarise with column names"
)

# Grouped Operations
expect_equal(
  mtcars %>% group_by(am, cyl) %>% summarise(meanMpg = mean(mpg)),
  structure(
    list(
      am = list(0, 1, 0, 1, 0, 1),
      cyl = list(4, 4, 6, 6, 8, 8),
      meanMpg = list(22.9, 28.075, 19.125, 20.5666666666667, 15.05, 15.4)
    ),
    class = "data.frame",
    row.names = c(NA, -6L)
  ),
  info = "Test grouped summarise"
)
