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

expect_equal(
  poorman:::select_positions(mtcars, "mpg"),
  c(mpg = 1),
  info = "Selecting by character string works"
)

expect_equal(
  poorman:::select_positions(mtcars, mpg),
  c(mpg = 1),
  info = "Selecting by symbol works"
)

expect_equal(
  poorman:::select_positions(mtcars, starts_with("m")),
  c(mpg = 1),
  info = "Selecting columns using an expression"
)

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

expect_error(
  poorman:::select_positions(mtcars, 100),
  info = "Out of range columns error"
)

expect_error(
  poorman:::select_positions(mtcars, TRUE),
  info = "Logical selections do not work"
)

# Expressions evaluating to column names are matched by name and are evaluated in the caller's environment (#118)
df <- data.frame(X = 1:5, Y1 = 1:5, Y2 = 1:5)
expect_equal(
  rename_with(df, toupper, names(df)[-1]),
  data.frame(X = 1:5, Y1 = 1:5, Y2 = 1:5) %>% rename(Y1 = Y1),
  info = "rename_with() accepts an expression returning column names"
)
expect_equal(
  colnames(rename_with(df, tolower, names(df)[-1])),
  c("X", "y1", "y2"),
  info = "rename_with() renames the columns returned by an expression"
)
expect_equal(
  lapply(c("X", "Y1"), function(x) colnames(select(df, all_of(x)))),
  list("X", "Y1"),
  info = "select() finds variables local to an anonymous function"
)

# As in dplyr, a selection which starts with a negation starts from every column, even when it deselects none
df <- data.frame(a = 1, b = 2)
expect_equal(select(df, -group_cols()), df, info = "-group_cols() on ungrouped data selects every column")
expect_equal(select(df, -starts_with("z")), df, info = "a negation which matches no column selects every column")
expect_equal(
  select(df, -starts_with("z"), a), df,
  info = "a selection which starts with a negation which matches no column selects every column"
)
expect_equal(select(df, !starts_with("z")), df, info = "a ! negation which matches no column selects every column")
rm(df)
