mt_gears <- mtcars %>% group_by(gear)

expect_equal(
  mtcars %>% mutate(n = n()),
  {
    res <- mtcars
    res$n <- 32L
    res
  },
  info = "n() works within mutate()"
)

expect_equal(
  mt_gears %>% mutate(n = n()) %>% ungroup(),
  {
    rows <- rownames(mtcars)
    res <- do.call(rbind, unname(lapply(split(mtcars, mtcars$gear), function(x) {
      x$n <- nrow(x)
      x
    })))
    res[rows[rows %in% rownames(res)], ]
  },
  info = "n() works within a grouped mutate()"
)

expect_equal(
  mtcars %>% filter(mpg < n() / 2),
  mtcars[mtcars$mpg < 16, ],
  info = "n() works within filter()"
)

expect_equal(
  mt_gears %>% filter(n() < 10) %>% ungroup(),
  {
    do.call(rbind, unname(lapply(split(mtcars, mtcars$gear), function(x) {
      x[nrow(x) < 10, ]
    })))
  },
  info = "n() works within a grouped filter()"
)

expect_equal(
  mtcars %>% summarise(n = n()),
  data.frame(n = 32),
  info = "n() works within summarise()"
)

expect_equal(
  mt_gears %>% summarise(n = n()) %>% ungroup(),
  data.frame(gear = c(3, 4, 5), n = c(15, 12, 5)),
  info = "n() works within a grouped summarise()"
)

expect_error(
  mtcars %>% select(n())
)

rm(mt_gears)
