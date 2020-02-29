expect_equal(
  mtcars %>% mutate(mpg2 = mpg * 2),
  {
    mtcars[, "mpg2"] <- mtcars$mpg * 2
    mtcars
  },
  info = "Test adding a new column"
)

expect_equal(
  mtcars %>% mutate(mpg2 = mpg * 2, mpg3 = mpg * 3),
  {
    mtcars[, "mpg2"] <- mtcars$mpg * 2
    mtcars[, "mpg3"] <- mtcars$mpg * 3
    mtcars
  },
  info = "Test adding multiple columns"
)

# Grouped Operations
expect_equal(
  mtcars %>% group_by(am, cyl) %>% mutate(mpg2 = mpg * 2) %>% ungroup(),
  do.call(rbind, unname(lapply(
    split(mtcars, list(mtcars$am , mtcars$cyl)),
    function(x) {
      x[, "mpg2"] <- x$mpg * 2
      x
    }
  ))),
  info = "Test grouped mutations"
)
