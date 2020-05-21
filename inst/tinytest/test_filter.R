expect_equal(
  mtcars %>% filter(am == 1),
  mtcars[mtcars$am == 1, ],
  info = "Test single logical filter"
)

expect_equal(
  mtcars %>% filter(mpg > 28 & am == 1),
  mtcars[mtcars$mpg > 28 & mtcars$am == 1, ],
  info = "Test multiple logical filters; sep = &"
)

expect_equal(
  mtcars %>% filter(mpg > 28, am == 1),
  mtcars[mtcars$mpg > 28 & mtcars$am == 1, ],
  info = "Test multiple logical filters; sep = ,"
)

expect_equal(
  mtcars %>% filter(cyl == 4 | cyl == 8),
  mtcars[mtcars$cyl == 4 | mtcars$cyl == 8, ],
  info = "Test multiple logical filters; sep = |"
)

expect_equal(
  mtcars %>% filter(!(cyl %in% c(4, 6)), am != 0),
  mtcars[!(mtcars$cyl %in% c(4, 6)) & mtcars$am != 0, ],
  info = "Test multiple logical filters; negation"
)

expect_equal(
  {
    sp1 <- "virginica"
    filter(iris, Species == sp1)
  },
  iris[iris$Species == "virginica", ],
  info = "Test filters work with variable inputs"
)

expect_equal(
  {
    sp2 <- levels(iris$Species)
    lapply(sp2, function(x) {
      filter(iris, Species == x)
    })
  },
  {
    res <- split(iris, iris$Species)
    names(res) <- NULL
    res
  },
  info = "Test filter works in an anonymous function (#9)"
)

expect_equal(
  {
    sp3 <- unique(as.character(iris$Species))
    lapply(sp2, function(x) {
      filter(iris, Species == x)
    })
  },
  {
    res <- split(iris, iris$Species)
    names(res) <- NULL
    res
  },
  info = "Test filter works in an anonymous function; factors (#9)"
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
