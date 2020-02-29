expect_equal(
  mtcars %>% slice(1:3),
  mtcars[1:3, ],
  info = "Test slicing rows with sequence"
)

expect_equal(
  mtcars %>% slice(1, 5, 9),
  mtcars[c(1, 5, 9), ],
  info = "Test slicing rows with random numbers"
)

expect_equal(
  mtcars %>% slice(1:100),
  mtcars,
  info = "Test that extra rows aren't returned if the user slices more rows than are available"
)

# Grouped Operations
expect_equal(
  mtcars %>% group_by(am, cyl) %>% slice(1:2) %>% ungroup(),
  do.call(rbind, unname(lapply(split(mtcars, list(mtcars$am, mtcars$cyl)), function(x) x[1:2, ]))),
  info = "Test that grouped slice operations work as expected"
)
