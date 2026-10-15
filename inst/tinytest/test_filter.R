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

expect_error(
  filter(.data = mtcars, mpg > 20, am = 1),
  info = "Check for named arguments works"
)

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
