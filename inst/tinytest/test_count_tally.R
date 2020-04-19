expect_equal(
  mtcars %>% count(cyl),
  data.frame(cyl = c(4, 6, 8), n = c(11L, 7L, 14L)),
  info = "count() works on a single grouping variable"
)

expect_equal(
  mtcars %>% group_by(gear) %>% count(cyl),
  structure(list(
    gear = c(3, 3, 3, 4, 4, 5, 5, 5), cyl = c(4, 6, 8, 4, 6, 4, 6, 8), n = c(1L, 2L, 12L, 8L, 4L, 2L, 1L, 2L)
  ), row.names = c(NA, 8L), class = c("grouped_data", "data.frame"), groups = "gear"),
  info = "count() works on a grouped data.frame"
)

expect_equal(
  mtcars %>% count(cyl, wt = am),
  data.frame(cyl = c(4, 6, 8), n = c(8, 3, 2)),
  info = "count() can perform weighted counts"
)

expect_equal(
  mtcars %>% count(cyl, sort = TRUE, name = "freq"),
  data.frame(cyl = c(8, 4, 6), freq = c(14, 11, 7)),
  info = "count() is able to sort and rename the frequency column"
)

expect_equal(
  mtcars %>% tally(),
  data.frame(n = 32),
  info = "tally() works on non-grouped data.frames"
)

expect_equal(
  mtcars %>% group_by(cyl) %>% tally(),
  data.frame(cyl = c(4, 6, 8), n = c(11L, 7L, 14L)),
  info = "tally() works on grouped data.frames"
)

expect_equal(
  mtcars %>% tally(wt = am),
  data.frame(n = 13),
  info = "tally() can perform weighted counts"
)

expect_equal(
  mtcars %>% group_by(cyl) %>% tally(wt = am),
  data.frame(cyl = c(4, 6, 8), n = c(8, 3, 2)),
  info = "tally() can perform weighted counts on grouped data.frames"
)

expect_equal(
  mtcars %>% group_by(cyl) %>% tally(sort = TRUE, name = "freq"),
  data.frame(cyl = c(8, 4, 6), freq = c(14L, 11L, 7L)),
  info = "tally() can sort and rename the frequency variable"
)

expect_equal(
  mtcars %>% add_count(cyl, wt = am, sort = TRUE, name = "freq"),
  {
    res <- mtcars
    res[, "freq"] <- c(3, 3, 8, 3, 2, 3, 2, 8, 8, 3, 3, 2, 2, 2, 2, 2, 2, 8, 8, 8, 8, 2, 2, 2, 2, 8, 8, 8, 2, 3, 2, 8)
    attr(res, "groups") <- "cyl"
    attr(res, "class") <- c("grouped_data", "data.frame")
    res
  },
  info = "add_count() returns the expected data.frame"
)

expect_equal(
  mtcars %>% group_by(cyl) %>% add_tally(wt = am, sort = TRUE, name = "freq"),
  {
    res <- mtcars
    res <- res[order(mtcars$cyl), ]
    res[, "freq"] <- c(8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 3, 3, 3, 3, 3, 3, 3, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2)
    attr(res, "groups") <- "cyl"
    attr(res, "class") <- c("grouped_data", "data.frame")
    res
  },
  info = "tally() returns the expected data.frame"
)

expect_equal(
  mtcars %>% group_by(cyl) %>% add_tally(),
  {
    res <- mtcars
    res <- res[order(mtcars$cyl), ]
    res[, "n"] <- c(
      11L, 11L, 11L, 11L, 11L, 11L, 11L, 11L, 11L, 11L, 11L, 7L, 7L, 7L, 7L, 7L, 7L, 7L, 14L, 14L, 14L, 14L, 14L, 14L,
      14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L
    )
    attr(res, "groups") <- "cyl"
    attr(res, "class") <- c("grouped_data", "data.frame")
    res
  }
)

expect_error(
  mtcars %>% count(cyl, name = c("name_one", "name_two"))
)

expect_error(
  {
    res <- mtcars
    colnames(res)[1] <- "n"
    res %>% tally()
  }
)

expect_message(
  {
    res <- mtcars
    colnames(res)[1] <- "n"
    res %>% add_tally(name = "freq")
  }
)
