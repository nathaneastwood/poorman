# Integer
expect_equal(
  poorman:::select_positions(mtcars, 1L),
  c(mpg = 1),
  info = "Integer selections work"
)

expect_equal(
  poorman:::select_positions(mtcars, 0L),
  integer(0),
  info = "Selecting the 0th integer column returns an empty data.frame"
)

# Numeric
expect_equal(
  poorman:::select_positions(mtcars, 1),
  c(mpg = 1),
  info = "Numeric selections work"
)
expect_equal(
  poorman:::select_positions(mtcars, 0),
  integer(0),
  info = "Selecting the 0th numeric column returns an empty data.frame"
)

# Character
expect_equal(
  poorman:::select_positions(mtcars, "mpg"),
  c(mpg = 1),
  info = "Selecting by character string works"
)

# Symbol
expect_equal(
  poorman:::select_positions(mtcars, mpg),
  c(mpg = 1),
  info = "Selecting by symbol works"
)

# Expression
expect_equal(
  poorman:::select_positions(mtcars, starts_with("m")),
  c(mpg = 1),
  info = "Selecting columns using an expression"
)

# Sequence
expect_equal(
  poorman:::select_positions(mtcars, 1:3),
  c(mpg = 1, cyl = 2, disp = 3),
  info = "Selecting with a numeric sequence"
)
expect_equal(
  poorman:::select_positions(mtcars, "mpg":"cyl"),
  c(mpg = 1, cyl = 2),
  info = "Selecting with a character sequence"
)
expect_equal(
  poorman:::select_positions(mtcars, mpg:cyl),
  c(mpg = 1, cyl = 2),
  info = "Selecting with a symbol sequence"
)
expect_equal(
  poorman:::select_positions(mtcars, -1:-3),
  c(hp = 4, drat = 5, wt = 6, qsec = 7, vs = 8, am = 9, gear = 10, carb = 11),
  info = "Dropping columns with a numeric sequence"
)
expect_equal(
  poorman:::select_positions(mtcars, -mpg:-cyl),
  c(disp = 3, hp = 4, drat = 5, wt = 6, qsec = 7, vs = 8, am = 9, gear = 10, carb = 11),
  info = "Dropping columns with a symbol sequence"
)
expect_equal(
  poorman:::select_positions(mtcars, -"mpg":-"cyl"),
  c(disp = 3, hp = 4, drat = 5, wt = 6, qsec = 7, vs = 8, am = 9, gear = 10, carb = 11),
  info = "Dropping columns with a charatcer sequence"
)

# Negated
expect_equal(
  poorman:::select_positions(mtcars, !mpg),
  c(cyl = 2, disp = 3, hp = 4, drat = 5, wt = 6, qsec = 7, vs = 8, am = 9, gear = 10, carb = 11),
  info = "Dropping columns with a symbol"
)
expect_equal(
  poorman:::select_positions(mtcars, !1),
  c(cyl = 2, disp = 3, hp = 4, drat = 5, wt = 6, qsec = 7, vs = 8, am = 9, gear = 10, carb = 11),
  info = "Dropping columns with a numeric"
)
expect_equal(
  poorman:::select_positions(mtcars, !"mpg"),
  c(cyl = 2, disp = 3, hp = 4, drat = 5, wt = 6, qsec = 7, vs = 8, am = 9, gear = 10, carb = 11),
  info = "Dropping columns with a character vector"
)
expect_equal(
  poorman:::select_positions(mtcars, !mpg:!cyl),
  c(disp = 3, hp = 4, drat = 5, wt = 6, qsec = 7, vs = 8, am = 9, gear = 10, carb = 11),
  info = "Dropping columns with symbol sequences"
)
expect_equal(
  poorman:::select_positions(mtcars, !"mpg":!"cyl"),
  c(disp = 3, hp = 4, drat = 5, wt = 6, qsec = 7, vs = 8, am = 9, gear = 10, carb = 11),
  info = "Dropping columns with a character sequence"
)
expect_equal(
  poorman:::select_positions(mtcars, !c(mpg, cyl)),
  c(disp = 3, hp = 4, drat = 5, wt = 6, qsec = 7, vs = 8, am = 9, gear = 10, carb = 11),
  info = "Dropping columns using a negated c()"
)

# Minus
expect_equal(
  poorman:::select_positions(mtcars, -1),
  c(cyl = 2, disp = 3, hp = 4, drat = 5, wt = 6, qsec = 7, vs = 8, am = 9, gear = 10, carb = 11),
  info = "Dropping columns with a numeric"
)
expect_equal(
  poorman:::select_positions(mtcars, -1L),
  c(cyl = 2, disp = 3, hp = 4, drat = 5, wt = 6, qsec = 7, vs = 8, am = 9, gear = 10, carb = 11),
  info = "Dropping columns with an integer"
)
expect_equal(
  poorman:::select_positions(mtcars, -mpg),
  c(cyl = 2, disp = 3, hp = 4, drat = 5, wt = 6, qsec = 7, vs = 8, am = 9, gear = 10, carb = 11),
  info = "Dropping columns with a symbol"
)
expect_equal(
  poorman:::select_positions(mtcars, -"mpg"),
  c(cyl = 2, disp = 3, hp = 4, drat = 5, wt = 6, qsec = 7, vs = 8, am = 9, gear = 10, carb = 11),
  info = "Dropping columns with a character vector"
)
expect_equal(
  poorman:::select_positions(mtcars, -starts_with("m")),
  c(cyl = 2, disp = 3, hp = 4, drat = 5, wt = 6, qsec = 7, vs = 8, am = 9, gear = 10, carb = 11),
  info = "Dropping columns using an expression"
)

# c()
expect_equal(
  poorman:::select_positions(mtcars, c(1, 2)),
  c(mpg = 1, cyl = 2),
  info = "Selecting columns with c() and numerics"
)
expect_equal(
  poorman:::select_positions(mtcars, c("mpg", "cyl")),
  c(mpg = 1, cyl = 2),
  info = "Selecting columns with c() and character strings"
)
expect_equal(
  poorman:::select_positions(mtcars, c(mpg, cyl)),
  c(mpg = 1, cyl = 2),
  info = "Selecting columns with c() and symbols"
)
expect_equal(
  poorman:::select_positions(mtcars, c(starts_with("m"), starts_with("c"))),
  c(mpg = 1, cyl = 2, carb = 11),
  info = "Selecting columns with c() and expressions"
)
expect_equal(
  poorman:::select_positions(mtcars, c(-mpg, -cyl)),
  c(disp = 3, hp = 4, drat = 5, wt = 6, qsec = 7, vs = 8, am = 9, gear = 10, carb = 11),
  info = "Dropping columns using c() and symbols"
)
expect_equal(
  poorman:::select_positions(mtcars, !c(mpg, cyl)),
  c(disp = 3, hp = 4, drat = 5, wt = 6, qsec = 7, vs = 8, am = 9, gear = 10, carb = 11),
  info = "Dropping columns using a negated c()"
)

# ()
expect_equal(
  poorman:::select_positions(mtcars, -(1:2)),
  c(disp = 3, hp = 4, drat = 5, wt = 6, qsec = 7, vs = 8, am = 9, gear = 10, carb = 11),
  info = "Dropping columns using a negative, bracketed sequence"
)
expect_equal(
  poorman:::select_positions(mtcars, !(1:2)),
  c(disp = 3, hp = 4, drat = 5, wt = 6, qsec = 7, vs = 8, am = 9, gear = 10, carb = 11),
  info = "Dropping columns using a negated, bracketed sequence"
)

# Multiple columns
expect_equal(
  poorman:::select_positions(mtcars, 1L, 2, "disp", "hp", starts_with("dr"), wt:qsec),
  c(mpg = 1, cyl = 2, disp = 3, hp = 4, drat = 5, wt = 6, qsec = 7),
  info = "Test selecting with a mixture of selection options"
)

expect_equal(
  poorman:::select_positions(mtcars, -2, 3),
  c(mpg = 1, disp = 3, hp = 4, drat = 5, wt = 6, qsec = 7, vs = 8, am = 9, gear = 10, carb = 11),
  info = "Mixture of negative and positives select only negatives"
)

expect_equal(
  poorman:::select_positions(mtcars, 3, -2),
  c(disp = 3),
  info = "Mixture of positives and negatives select only negatives"
)

# NULL
expect_equal(
  poorman:::select_positions(mtcars, NULL),
  integer(0),
  info = "NULL returns zero column positions"
)

expect_equal(
  poorman:::select_positions(mtcars, am, NULL, cyl),
  c("am" = 9, "cyl" = 2),
  info = "combinations of NULL and other parameter names ignore the NULLs"
)

# Errors
expect_error(
  poorman:::select_positions(mtcars, 100),
  info = "Out of range columns error"
)

expect_error(
  poorman:::select_positions(mtcars, TRUE),
  info = "Logical selections do not work"
)
