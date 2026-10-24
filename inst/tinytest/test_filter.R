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

expect_equal(
  filter(.data = mtcars),
  mtcars,
  info = "filter() returns the input data if no parameters are given"
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

expect_equal(
  mtcars %>% group_by(cyl, am) %>% filter(cyl %in% c(4, 8), .preserve = TRUE) %>% group_rows(),
  list(
    c(4L, 5L, 15L), c(1L, 12L, 13L, 14L, 20L, 21L, 22L, 25L), integer(0), integer(0),
    c(2L, 3L, 6L, 7L, 8L, 9L, 10L, 11L, 16L, 17L, 18L, 19L), 23:24
  ),
  info = ".preserve = TRUE returns the correct row indicies"
)

expect_equal(
  mtcars %>% group_by(cyl, am) %>% filter(cyl %in% c(4, 8), .preserve = FALSE) %>% group_rows(),
  list(
    c(4L, 5L, 15L), c(1L, 12L, 13L, 14L, 20L, 21L, 22L, 25L),
    c(2L, 3L, 6L, 7L, 8L, 9L, 10L, 11L, 16L, 17L, 18L, 19L), 23:24
  ),
  info = ".preserve = FALSE returns the correct row indicies"
)

# Errors

named_error <- function(suggestion) {
  paste0(
    "We detected a named input.\n",
    "i This usually means that you've used `=` instead of `==`.\n",
    "i Did you mean `", suggestion, "`?"
  )
}
named_message <- function(expr) tryCatch(expr, error = conditionMessage)
expect_equal(
  named_message(filter(.data = mtcars, mpg > 20, am = 1)),
  named_error("am == 1"),
  info = "filter() rejects a named condition and suggests `==`"
)
expect_equal(
  named_message(filter(group_by(mtcars, cyl), am = 1, gear = 4)),
  named_error("am == 1"),
  info = "grouped filter() names the first named condition"
)
expect_equal(
  named_message(filter(iris, Species = "setosa")),
  named_error("Species == \"setosa\""),
  info = "filter() quotes a string in the suggestion"
)
expect_equal(
  named_message(filter(mtcars, z = am == 1)),
  named_error("z == am == 1"),
  info = "filter() suggests `==` for a named expression"
)
expect_equal(
  named_message({
    k <- 4
    filter(mtcars, cyl = k)
  }),
  named_error("cyl == k"),
  info = "filter() suggests `==` for a named variable"
)
expect_equal(
  named_message(filter(mtcars, cyl = c(4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24, 26, 28, 30, 32, 34, 36, 38, 40))),
  named_error("cyl == c(...)"),
  info = "filter() abbreviates a long expression in the suggestion"
)
expect_equal(filter(mtcars, a = TRUE), mtcars, info = "filter() accepts a named logical constant")
expect_equal(nrow(filter(mtcars, a = NA)), 0L, info = "filter() accepts a named missing value")
rm(named_error, named_message)

# Grouped filter ----------------------------------------------------------------

expect_equal(
  ungroup(filter(group_by(data.frame(g = c(1, 1, 2)), g), g > 1)),
  data.frame(g = 2, row.names = 3L),
  info = "filter() returns a data.frame for single column grouped data"
)

df <- data.frame(g = c("a", NA, "a", NA), v = 1:4, stringsAsFactors = FALSE)
expect_equal(
  ungroup(filter(group_by(df, g), v > 1)),
  df[2:4, ],
  info = "filter() keeps rows with missing grouping values"
)

df <- data.frame(g1 = numeric(0), g2 = integer(0), v = integer(0))
expect_equal(
  nrow(filter(group_by(df, g1, g2, .drop = FALSE), v > 1)),
  0L,
  info = "filter() works on empty grouped data"
)

rm(df)

# Data frame and matrix results
df <- data.frame(g = c(2, 1, 2, NA), v = 1:4)
expect_warning(
  res <- filter(df, across(c(g, v), ~ !is.na(.x))),
  "Using `across\\(\\)` in `filter\\(\\)` is deprecated",
  info = "filter() warns that across() is deprecated"
)
expect_equal(res, df[1:3, ], info = "filter() combines the columns of across() with &")
expect_warning(
  res <- filter(df, data.frame(a = v > 1, b = v < 4)),
  "data frame results in `filter\\(\\)` are deprecated",
  info = "filter() warns that data frame results are deprecated"
)
expect_equal(res, df[2:3, ], info = "filter() combines the columns of a data frame result with &")
expect_equal(
  suppressWarnings(filter(df, across(starts_with("z"), ~ .x > 1))),
  df,
  info = "filter() keeps every row for a data frame result without columns"
)
expect_equal(
  filter(df, matrix(c(TRUE, FALSE, TRUE, FALSE), 4, 1)),
  df[c(1, 3), ],
  info = "filter() accepts a one column matrix"
)
expect_error(
  filter(df, matrix(TRUE, 4, 2)),
  "must be a logical vector, not a logical\\[,2\\]",
  info = "filter() rejects a matrix with several columns"
)
gf <- group_by(df, g)
warnings <- character()
res <- withCallingHandlers(
  filter(gf, across(v, ~ .x > 1)),
  warning = function(w) {
    warnings <<- c(warnings, conditionMessage(w))
    invokeRestart("muffleWarning")
  }
)
expect_equal(length(warnings), 1L, info = "grouped filter() warns once about data frame results")
expect_equal(res$v, 2:4, info = "grouped filter() combines the columns of across() with &")
rm(df, gf, res, warnings)

# Condition types
df <- data.frame(v = 1:3, ok = c(TRUE, FALSE, NA), d = c(0.5, 1, 2))
expect_equal(filter(df, ok), df[1, ], info = "filter() accepts a logical column")
expect_equal(filter(group_by(df, v), ok)$v, 1L, info = "grouped filter() accepts a logical column")
expect_error(filter(df, d), "Input `..1` must be a logical vector, not a double", info = "filter() rejects a numeric column")
expect_error(filter(df, v + 1), "Input `..1` must be a logical vector, not a double", info = "filter() rejects numeric results")
expect_error(filter(df, 1), "Input `..1` must be a logical vector, not a double", info = "filter() rejects numeric constants")
rm(df)

# Grouped engine ---------------------------------------------------------------

df <- data.frame(g = c("b", "a", "b", "c", "a"), x = 1:5, y = c(2, 4, 1, 3, 5), stringsAsFactors = FALSE)
gf <- group_by(df, g)
filter_error <- function(expr) tryCatch(expr, error = conditionMessage)

expect_equal(
  filter_error(filter(gf, c(TRUE, FALSE))),
  paste0(
    "Problem while computing `..1 = c(TRUE, FALSE)`.\n",
    "x Input `..1` must be of size 1, not size 2.\n",
    "i The error occurred in group 3: g = \"c\"."
  ),
  info = "grouped filter() checks that each condition has the size of its group or size one"
)
expect_equal(
  filter_error(filter(gf, logical(0))),
  paste0(
    "Problem while computing `..1 = logical(0)`.\n",
    "x Input `..1` must be of size 2 or 1, not size 0.\n",
    "i The error occurred in group 1: g = \"a\"."
  ),
  info = "grouped filter() rejects a condition of size zero"
)
expect_equal(
  filter_error(filter(df, c(TRUE, FALSE))),
  "Problem while computing `..1 = c(TRUE, FALSE)`.\nx Input `..1` must be of size 5 or 1, not size 2.",
  info = "filter() checks that each condition has the size of the data or size one"
)
expect_equal(
  filter_error(filter(gf, 1:3)),
  paste0(
    "Problem while computing `..1 = 1:3`.\n",
    "x Input `..1` must be of size 2 or 1, not size 3.\n",
    "i The error occurred in group 1: g = \"a\"."
  ),
  info = "filter() checks the size of a condition before its type"
)
expect_equal(
  filter_error(filter(gf, x > 1, y)),
  paste0(
    "Problem while computing `..2 = y`.\n",
    "x Input `..2` must be a logical vector, not a double.\n",
    "i The error occurred in group 1: g = \"a\"."
  ),
  info = "grouped filter() names the group of a condition which is not logical"
)
expect_equal(
  filter_error(filter(gf, NULL)),
  paste0(
    "Problem while computing `..1 = NULL`.\n",
    "x Input `..1` must be of size 2 or 1, not size 0.\n",
    "i The error occurred in group 1: g = \"a\"."
  ),
  info = "filter() treats a NULL condition as size zero"
)
expect_silent(
  expect_error(filter(gf, data.frame()), "must be of size 2 or 1, not size 0"),
  info = "filter() rejects a data frame without columns or rows, without a deprecation warning"
)
expect_silent(
  expect_error(filter(gf, data.frame(a = x)), "x Input `..1$a` must be a logical vector, not a integer.", fixed = TRUE),
  info = "filter() checks the type of each column of a data frame result before the deprecation warning"
)

warnings <- character()
res <- withCallingHandlers(
  tryCatch(filter(gf, data.frame(a = c(TRUE, FALSE))), error = conditionMessage),
  warning = function(w) {
    warnings <<- c(warnings, conditionMessage(w))
    invokeRestart("muffleWarning")
  }
)
expect_equal(length(warnings), 1L, info = "grouped filter() warns about a data frame result before a later error")
expect_true(
  grepl("must be of size 1, not size 2.\ni The error occurred in group 3", res, fixed = TRUE),
  info = "grouped filter() checks the size of a data frame result"
)
warnings <- character()
res <- withCallingHandlers(
  filter(gf, data.frame(a = x > 1), data.frame(b = y > 1)),
  warning = function(w) {
    warnings <<- c(warnings, conditionMessage(w))
    invokeRestart("muffleWarning")
  }
)
expect_equal(length(warnings), 2L, info = "grouped filter() warns once for each condition with a data frame result")
expect_equal(res$x, c(2L, 4L, 5L), info = "grouped filter() keeps the rows where every data frame column is TRUE")
expect_equal(
  suppressWarnings(filter(gf, data.frame(a = c(NA, TRUE)[seq_len(n())], b = TRUE)))$x,
  c(3L, 5L),
  info = "grouped filter() drops rows where a data frame column is NA"
)
expect_equal(
  suppressWarnings(filter(gf, across(starts_with("z"), ~ .x > 1)))$x,
  1:5,
  info = "grouped filter() keeps every row for across() without columns"
)
expect_equal(
  mutate(df, across(starts_with("z"))),
  df,
  info = "mutate() adds no columns for across() without columns"
)

expect_equal(
  ungroup(filter(gf, .data$x > 2)),
  df[3:5, ],
  info = "grouped filter() finds columns through .data"
)
expect_equal(filter(df, .data[["x"]] > 2), df[3:5, ], info = "filter() finds columns through .data")
filter_above <- function(data, k) filter(data, x > k)
expect_equal(
  ungroup(filter_above(gf, 2)),
  df[3:5, ],
  info = "grouped filter() finds the local variables of the caller"
)
expect_equal(nrow(filter(df[0, ], TRUE)), 0L, info = "filter() of data without rows returns no rows")
expect_equal(nrow(filter(gf[0, ], TRUE)), 0L, info = "grouped filter() of data without rows returns no rows")

res <- filter(gf, x > 1)
expect_equal(res$x, 2:5, info = "grouped filter() keeps the rows in their original order")
expect_equal(rownames(res), c("2", "3", "4", "5"), info = "grouped filter() keeps the row names of the rows")
expect_equal(
  group_rows(res), list(c(1L, 4L), 2L, 3L), info = "grouped filter() gives the rows of the groups in the result"
)
expect_equal(
  group_rows(filter(gf, x > 3, .preserve = TRUE)),
  list(2L, integer(), 1L),
  info = "grouped filter() with .preserve = TRUE keeps the empty groups"
)
expect_equal(
  group_rows(filter(gf, cur_group_id() == 2L)),
  list(1:2),
  info = "grouped filter() drops the empty groups"
)

seen <- character()
invisible(filter(
  gf,
  {
    seen <<- c(seen, paste0("a", cur_group_id()))
    TRUE
  },
  {
    seen <<- c(seen, paste0("b", cur_group_id()))
    TRUE
  }
))
expect_equal(
  seen,
  c("a1", "b1", "a2", "b2", "a3", "b3"),
  info = "grouped filter() evaluates every condition for a group before the next group"
)

fd <- data.frame(
  g = factor(c("a", "a", "b"), levels = c("a", "b", "c")), h = c("p", "q", "p"), x = 1:3, stringsAsFactors = FALSE
)
expect_error(
  filter(group_by(fd, g, .drop = FALSE), if (n() == 0L) stop("boom") else TRUE),
  "boom",
  info = "grouped filter() evaluates empty groups"
)
res <- filter(group_by(fd, h, g, .drop = FALSE), x > 2)
expect_equal(
  group_keys(res),
  data.frame(
    h = rep(c("p", "q"), each = 3L), g = factor(rep(c("a", "b", "c"), 2L), levels = c("a", "b", "c")),
    stringsAsFactors = FALSE
  ),
  info = "grouped filter() with .drop = FALSE keeps every group"
)
expect_equal(
  group_rows(res),
  list(integer(), 1L, integer(), integer(), integer(), integer()),
  info = "grouped filter() with .drop = FALSE gives the rows of the groups in the result"
)
rm(df, gf, fd, res, seen, warnings, filter_error, filter_above)
