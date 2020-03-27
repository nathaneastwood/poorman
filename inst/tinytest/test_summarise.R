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
      am = c(0, 0, 0, 1, 1, 1),
      cyl = c(4, 6, 8, 4, 6, 8),
      meanMpg = c(22.9, 19.125, 15.05, 28.075, 20.5666666666667, 15.4)
    ),
    row.names = c(NA, 6L),
    class = c("grouped_data", "data.frame"),
    groups = c("am", "cyl")
  ),
  info = "Test grouped summarise"
)

expect_equal(
  mtcars %>% group_by(am, cyl, gear) %>% summarise(meanMpg = mean(mpg), sumDisp = sum(disp)),
  structure(
    list(
      am = c(0, 0, 0, 0, 0, 1, 1, 1, 1, 1),
      cyl = c(4, 4, 6, 6, 8, 4, 4, 6, 6, 8),
      gear = c(3, 4, 3, 4, 3, 4, 5, 4, 5, 5),
      meanMpg = c(21.5, 23.6, 19.75, 18.5, 15.05, 28.0333333333333, 28.2, 21, 19.7, 15.4),
      sumDisp = c(120.1, 287.5, 483, 335.2, 4291.4, 533.5, 215.4, 320, 145, 652)
    ),
    row.names = c(NA, 10L),
    class = c("grouped_data", "data.frame"),
    groups = c("am", "cyl", "gear")
  ),
  info = "Test multiple groups and multiple summary functions"
)
