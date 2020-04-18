expect_equal(
  desc(1:10),
  -1:-10,
  info = "desc() returns negative vectors"
)

expect_equal(
  mtcars %>% arrange(desc(mpg)),
  mtcars[order(-mtcars$mpg), ],
  info = "desc() arranges a data.frame in descending order of the chosen column"
)
