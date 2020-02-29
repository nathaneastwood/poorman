expect_equal(
  mtcars %>% transmute(mpg2 = mpg * 2),
  {
    mtcars[, "mpg2"] <- mtcars$mpg * 2
    mtcars[, "mpg2", drop = FALSE]
  },
  info = "Transmute single column"
)

expect_equal(
  mtcars %>% transmute(mpg2 = mpg * 2, mpg3 = mpg * 3),
  {
    mtcars[, "mpg2"] <- mtcars$mpg * 2
    mtcars[, "mpg3"] <- mtcars$mpg * 3
    mtcars[, c("mpg2", "mpg3")]
  },
  info = "Transmute multiple columns"
)

expect_equal(
  mtcars %>% group_by(am) %>% transmute(sumMpg = sum(mpg)),
  do.call(rbind, unname(lapply(
    split(mtcars, mtcars$am),
    function(x) {
      x[, "sumMpg"] <- sum(x$mpg)
      x[, "sumMpg", drop = FALSE]
    }
  ))),
  info = "Transmute grouped dataframe"
)
