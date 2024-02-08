expect_equal(
  mtcars %>% nest_by(am),
  {
    res <- data.frame(am = c(0L, 1L))
    res[["data"]] <- list(mtcars[mtcars$am == 0, -9], mtcars[mtcars$am == 1, -9])
    res %>% group_by(am)
  },
  info = "nest_by() takes group names"
)

expect_equal(
  mtcars |> select(mpg, cyl, am) |> nest_by(am, cyl),
  {
    res <- data.frame(
      am = c(0, 0, 0, 1, 1, 1),
      cyl = c(4, 6, 8, 4, 6, 8)
    )
    res[["data"]] <- list(
      mtcars[mtcars$am == 0 & mtcars$cyl == 4, "mpg", drop = FALSE],
      mtcars[mtcars$am == 0 & mtcars$cyl == 6, "mpg", drop = FALSE],
      mtcars[mtcars$am == 0 & mtcars$cyl == 8, "mpg", drop = FALSE],
      mtcars[mtcars$am == 1 & mtcars$cyl == 4, "mpg", drop = FALSE],
      mtcars[mtcars$am == 1 & mtcars$cyl == 6, "mpg", drop = FALSE],
      mtcars[mtcars$am == 1 & mtcars$cyl == 8, "mpg", drop = FALSE]
    )
    group_by(res, am)
  },
  info = "nest_by() works with only one non-group column (#124)"
)

expect_equal(
  mtcars %>% group_by(am) %>% nest_by(),
  {
    res <- data.frame(am = c(0L, 1L))
    res[["data"]] <- list(mtcars[mtcars$am == 0, -9], mtcars[mtcars$am == 1, -9])
    res %>% group_by(am)
  },
  info = "nest_by() works on grouped data sets"
)

expect_error(
  mtcars %>% nest_by(am, .keep = "true"),
  info = "`.keep` should be `logical(1)`"
)

expect_error(
  mtcars %>% nest_by(am, .key = TRUE),
  info = "`.key` should be `character(1)`"
)

expect_error(
  mtcars %>% group_by(am) %>% nest_by(cyl),
  info = "Can't re-group while nesting. Either `ungroup()` first or don't supply arguments to `nest_by()`"
)
