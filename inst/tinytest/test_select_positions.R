# Integer
expect_equal(
  mtcars %>% select(1L),
  mtcars[, 1L, drop = FALSE],
  info = "Integer selections work"
)

expect_equal(
  mtcars %>% select(0L),
  mtcars[, 0L],
  info = "Selecting the 0th integer column returns an empty data.frame"
)

# Numeric
expect_equal(
  mtcars %>% select(1),
  mtcars[, 1, drop = FALSE],
  info = "Numeric selections work"
)
expect_equal(
  mtcars %>% select(0),
  mtcars[, 0],
  info = "Selecting the 0th numeric column returns an empty data.frame"
)

# Logical
expect_error(
  mtcars %>% select(TRUE),
  info = "Logical selections do not work"
)

# Character
expect_equal(
  mtcars %>% select("mpg"),
  mtcars[, "mpg", drop = FALSE],
  info = "Selecting by character string works"
)

# Symbol
expect_equal(
  mtcars %>% select(mpg),
  mtcars[, "mpg", drop = FALSE],
  info = "Selecting by symbol works"
)
# expect_equal(
#   {
#     var <- "mpg"
#     mtcars %>% select(var)
#   },
#   mtcars[, "mpg", drop = FALSE],
#   info = "Selecting by a variable works"
# )

# Expression
expect_equal(
  mtcars %>% select(starts_with("m")),
  mtcars[, 1, drop = FALSE],
  info = "Selecting columns using an expression"
)

# Sequence
expect_equal(
  mtcars %>% select(1:3),
  mtcars[, 1:3],
  info = "Selecting with a numeric sequence"
)
expect_equal(
  mtcars %>% select("mpg":"cyl"),
  mtcars[, 1:2],
  info = "Selecting with a character sequence"
)
expect_equal(
  mtcars %>% select(mpg:cyl),
  mtcars[, 1:2],
  info = "Selecting with a symbol sequence"
)
expect_equal(
  mtcars %>% select(-1:-3),
  mtcars[, -1:-3],
  info = "Dropping columns with a numeric sequence"
)
expect_equal(
  mtcars %>% select(-mpg:-cyl),
  mtcars[, -1:-2],
  info = "Dropping columns with a symbol sequence"
)
expect_equal(
  mtcars %>% select(-"mpg":-"cyl"),
  mtcars[, -1:-2],
  info = "Dropping columns with a charatcer sequence"
)

# Negated
expect_equal(
  mtcars %>% select(!mpg),
  mtcars[, -1],
  info = "Dropping columns with a symbol"
)
expect_equal(
  mtcars %>% select(!1),
  mtcars[, -1],
  info = "Dropping columns with a numeric"
)
expect_equal(
  mtcars %>% select(!"mpg"),
  mtcars[, -1],
  info = "Dropping columns with a character vector"
)
expect_equal(
  mtcars %>% select(!mpg:!cyl),
  mtcars[, -1:-2],
  info = "Dropping columns with symbol sequences"
)
expect_equal(
  mtcars %>% select(!"mpg":!"cyl"),
  mtcars[, -1:-2],
  info = "Dropping columns with a character sequence"
)
expect_equal(
  mtcars %>% select(!c(mpg, cyl)),
  mtcars[, -(1:2)],
  info = "Dropping columns using a negated c()"
)

# Minus
expect_equal(
  mtcars %>% select(-1),
  mtcars[, -1],
  info = "Dropping columns with a numeric"
)
expect_equal(
  mtcars %>% select(-1L),
  mtcars[, -1L],
  info = "Dropping columns with an integer"
)
expect_equal(
  mtcars %>% select(-mpg),
  mtcars[, -1],
  info = "Dropping columns with a symbol"
)
expect_equal(
  mtcars %>% select(-"mpg"),
  mtcars[, -1],
  info = "Dropping columns with a character vector"
)
expect_equal(
  mtcars %>% select(-starts_with("m")),
  mtcars[, -1],
  info = "Dropping columns using an expression"
)

# c()
expect_equal(
  mtcars %>% select(c(1, 2)),
  mtcars[, c(1, 2)],
  info = "Selecting columns with c() and numerics"
)
expect_equal(
  mtcars %>% select(c("mpg", "cyl")),
  mtcars[, c("mpg", "cyl")],
  info = "Selecting columns with c() and character strings"
)
expect_equal(
  mtcars %>% select(c(mpg, cyl)),
  mtcars[, c("mpg", "cyl")],
  info = "Selecting columns with c() and symbols"
)
expect_equal(
  mtcars %>% select(c(starts_with("m"), starts_with("c"))),
  mtcars[, c("mpg", "cyl", "carb")],
  info = "Selecting columns with c() and expressions"
)
expect_equal(
  mtcars %>% select(c(-mpg, -cyl)),
  mtcars[, c(-1, -2)],
  info = "Dropping columns using c() and symbols"
)
expect_equal(
  mtcars %>% select(!c(mpg, cyl)),
  mtcars[, -(1:2)],
  info = "Dropping columns using a negated c()"
)

# ()
expect_equal(
  mtcars %>% select(-(1:2)),
  mtcars[, -(1:2)],
  info = "Dropping columns using a negative, bracketed sequence"
)
expect_equal(
  mtcars %>% select(!(1:2)),
  mtcars[, -(1:2)],
  info = "Dropping columns using a negated, bracketed sequence"
)

# Multiple columns
expect_equal(
  mtcars %>% select(1L, 2, "disp", "hp", starts_with("dr"), wt:qsec),
  mtcars[, c("mpg", "cyl", "disp", "hp", "drat", "wt", "qsec")],
  info = "Test selecting with a mixture of selection options"
)
