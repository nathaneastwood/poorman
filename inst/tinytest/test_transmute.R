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

tm_df <- data.frame(x = 1:3, g = c("a", "a", "b"))
tm_fn <- function(data) {
  k <- 10L
  transmute(data, z = x + k)
}
expect_equal(
  tm_fn(tm_df),
  data.frame(z = 11:13),
  info = "transmute() finds variables in the calling environment"
)
expect_equal(
  ungroup(tm_fn(group_by(tm_df, g))),
  data.frame(g = c("a", "a", "b"), z = 11:13),
  info = "transmute() finds variables in the calling environment on grouped data"
)

ce_nm <- "tot"
expect_equal(
  transmute(data.frame(y = c(2, 4)), "{ce_nm}" := y * 2), # nolint: object_name_linter.
  data.frame(tot = c(4, 8)),
  info = "transmute() names := arguments"
)
