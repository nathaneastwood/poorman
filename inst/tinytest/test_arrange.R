# Test single column arrangement
expect_equal(
  mtcars %>% arrange(mpg),
  mtcars[order(mtcars$mpg), ],
  info = "Test ascending column arrangement"
)

expect_equal(
  mtcars %>% arrange(-mpg),
  mtcars[order(-mtcars$mpg), ],
  info = "Test descending column arrangement"
)

expect_equal(
  mtcars %>% arrange(cyl, mpg),
  mtcars[order(mtcars$cyl, mtcars$mpg), ],
  info = "Test multiple column arrangement"
)

expect_equal(
  mtcars %>% arrange(cyl, -mpg),
  mtcars[order(mtcars$cyl -mtcars$mpg), ],
  info = "Test multiple ascending and descending column arrangement"
)

# Grouped Operations
expect_equal(
  mtcars %>% group_by(cyl) %>% arrange(mpg) %>% ungroup(),
  do.call(rbind, unname(lapply(split(mtcars, mtcars$cyl), function(x) x[order(x$mpg), ]))),
  info = "Test grouped arrangement"
)
