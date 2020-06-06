res <- split(mtcars, mtcars$cyl)
names(res) <- NULL
expect_equal(
  mtcars %>% group_by(cyl) %>% group_split(),
  res,
  info = "group_split() works"
)

expect_equal(
  mtcars %>% group_split(cyl),
  res,
  info = "Non-grouped group_split() works"
)
rm(res)

expect_equal(
  mtcars %>% group_by(cyl, am) %>% group_split(.keep = FALSE),
  {
    res <- split(mtcars, list(mtcars$cyl, mtcars$am))
    res <- lapply(res, function(x) x[, !colnames(x) %in% c("cyl", "am")])
    names(res) <- NULL
    res
  },
  info = ".keep = FALSE drops the grouping columns"
)

expect_equal(
  mtcars %>% group_by(cyl, am, gear) %>% group_split(),
  {
    res <- split(mtcars, list(mtcars$cyl, mtcars$am, mtcars$gear))
    names(res) <- NULL
    any_empty <- unlist(lapply(res, function(x) !(nrow(x) == 0L)))
    res[any_empty]
  },
  info = "Empty group combinations are dropped"
)

expect_equal(
  mtcars %>% group_split(),
  list(mtcars),
  info = "Non-grouped data with no groups given returns the whole dataset as a list"
)

expect_equal(
  mtcars %>% group_by(cyl, am) %>% group_keys(),
  structure(
    list(cyl = c(4, 4, 6, 6, 8, 8), am = c(0, 1, 0, 1, 0, 1)),
    row.names = c(NA, -6L),
    class = "data.frame"
  ),
  info = "group_keys() works"
)

expect_warning(
  mtcars %>% group_by(am) %>% group_split(cyl),
  info = "group_split() warns if data are already grouped"
)
