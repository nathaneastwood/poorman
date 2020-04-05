expect_error(
  mtcars %>% group_by(fake_column_name)
)

expect_equal(
  suppressMessages(mtcars %>% group_by(am, cyl) %>% select(mpg)),
  {
    res <- mtcars[, c("am", "cyl", "mpg")]
    class(res) <- c("grouped_data", "data.frame")
    attr(res, "groups") <- c("am", "cyl")
    res
  },
  info = "Test that groups persist in select() when no groups are selected"
)

expect_equal(
  suppressMessages(mtcars %>% group_by(am, cyl) %>% select(mpg, cyl)),
  {
    res <- mtcars[, c("am", "mpg", "cyl")]
    class(res) <- c("grouped_data", "data.frame")
    attr(res, "groups") <- c("am", "cyl")
    res
  },
  info = "Test that groups persist in select() when only some groups are selected"
)

expect_equal(
  mtcars %>% group_by(am, cyl) %>% relocate(gear, .before = mpg),
  {
    res <- mtcars[, c("gear", colnames(mtcars)[!colnames(mtcars) %in% "gear"])]
    class(res) <- c("grouped_data", "data.frame")
    attr(res, "groups") <- c("am", "cyl")
    res
  },
  info = "Test that groups persist in relocate()"
)

expect_equal(
  mtcars %>% group_by(am, cyl) %>% rename(Gears = gear),
  {
    res <- mtcars
    colnames(res)[10] <- "Gears"
    class(res) <- c("grouped_data", "data.frame")
    attr(res, "groups") <- c("am", "cyl")
    res
  },
  info = "Test that groups persist in rename()"
)
