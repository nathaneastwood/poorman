expect_equal(
  transmute(.data = mtcars, -mpg, neg_am = -am),
  {
    mtcars[["-mpg"]] <- mtcars$mpg * -1
    mtcars[, "neg_am"] <- mtcars$am * -1
    mtcars[, c("-mpg", "neg_am")]
  },
  info = "Transmute works with non-named parameters"
)

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
  mtcars %>% group_by(am) %>% transmute(sumMpg = sum(mpg)) %>% colnames(),
  c("am", "sumMpg"),
  info = "transmute() keeps groups"
)
