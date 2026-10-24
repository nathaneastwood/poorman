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
    res <- split(mtcars, list(mtcars$am, mtcars$cyl))
    res <- lapply(res, function(x) x[, !colnames(x) %in% c("cyl", "am")])
    names(res) <- NULL
    res
  },
  info = ".keep = FALSE drops the grouping columns"
)

expect_equal(
  mtcars %>% group_by(cyl, am, gear) %>% group_split(),
  {
    res <- split(mtcars, list(mtcars$gear, mtcars$am, mtcars$cyl))
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

df <- data.frame(f = factor(c("a", "b"), levels = c("a", "b", "c")), x = 1:2)
gf <- group_by(df, f, .drop = FALSE)
expect_equal(
  group_keys(gf),
  data.frame(f = factor(c("a", "b", "c"))),
  info = "group_keys() includes empty groups"
)
expect_equal(
  vapply(group_split(gf), nrow, integer(1)),
  c(1L, 1L, 0L),
  info = "group_split() includes empty groups"
)

df <- data.frame(g = c("a", NA, "a"), x = 1:3, stringsAsFactors = FALSE)
expect_equal(
  lapply(group_split(group_by(df, g)), function(x) x$x),
  list(c(1L, 3L), 2L),
  info = "group_split() keeps rows with missing grouping values"
)

gf <- group_by(mtcars, cyl, am)
expect_equal(
  do.call(rbind, lapply(group_split(gf), function(x) x[1L, c("cyl", "am")])),
  {
    res <- group_keys(gf)
    rownames(res) <- rownames(do.call(rbind, lapply(group_split(gf), function(x) x[1L, c("cyl", "am")])))
    res
  },
  info = "group_split() returns the groups in the same order as group_keys()"
)

invisible(group_keys(gf))
expect_error(n(), info = "group_keys() does not leave its data in the context")

rm(df, gf, res)

fe_df <- data.frame(g = c(1, 1, 2), x = 1:3)
fe_k <- 100L
fe_fn <- function(data) {
  fe_k <- 2L
  nm <- "new"
  list(
    lapply(group_split(data, h = x %% fe_k), `[[`, "x"),
    names(group_split(data, "{nm}" := x)[[1L]]) # nolint: object_name_linter.
  )
}
expect_equal(
  fe_fn(fe_df),
  list(list(2L, c(1L, 3L)), c("g", "x", "new")),
  info = "group_split() uses a local variable over a global one, also in a := template"
)
