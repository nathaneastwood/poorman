expect_equal(
  mtcars %>% filter(am == 1),
  mtcars[mtcars$am == 1, ],
  info = "Test logical filters 1"
)

expect_equal(
  mtcars %>% filter(mpg > 28 & am == 1),
  mtcars[mtcars$mpg > 28 & mtcars$am == 1, ],
  info = "Test logical filters 2"
)

expect_equal(
  mtcars %>% filter(mpg > 28, am == 1),
  mtcars[mtcars$mpg > 28 & mtcars$am == 1, ],
  info = "Test logical filters 3"
)

expect_equal(
  mtcars %>% filter(cyl == 4 | cyl == 8),
  mtcars[mtcars$cyl == 4 | mtcars$cyl == 8, ],
  info = "Test logical filters 4"
)

expect_equal(
  mtcars %>% filter(!(cyl %in% c(4, 6)), am != 0),
  mtcars[!(mtcars$cyl %in% c(4, 6)) & mtcars$am != 0, ],
  info = "Test logical filters 5"
)

# Grouped Operations
expect_equal(
  mtcars %>% group_by(carb) %>% filter(any(mpg > 28)) %>% ungroup(),
  {
    rows <- rownames(mtcars)
    res <- do.call(rbind, unname(lapply(split(mtcars, mtcars$carb), function(x) x[any(x$mpg > 28), ])))
    res[rows[rows %in% rownames(res)], ]
  },
  info = "Test grouped filters"
)
