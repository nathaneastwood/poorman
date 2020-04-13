expect_equal(
  mtcars %>% relocate(drat),
  mtcars[, c("drat", colnames(mtcars)[!colnames(mtcars) %in% "drat"])],
  info = "Test default relocate() moves column to the start of the data.frame"
)

expect_equal(
  mtcars %>% relocate(drat, .before = cyl),
  mtcars[, c("mpg", "drat", "cyl", "disp", "hp", "wt", "qsec", "vs", "am", "gear", "carb")],
  info = "Test .before functionality for single variable"
)

expect_equal(
  mtcars %>% relocate(drat, .after = cyl),
  mtcars[, c("mpg", "cyl", "drat", "disp", "hp", "wt", "qsec", "vs", "am", "gear", "carb")],
  info = "Test .after functionality for single variable"
)

expect_equal(
  mtcars %>% relocate(drat, gear, .after = cyl),
  mtcars[, c("mpg", "cyl", "drat", "gear", "disp", "hp", "wt", "qsec", "vs", "am", "carb")],
  info = "Test .after functionality for multiple variables"
)

expect_equal(
  iris %>% relocate(contains("Petal"), .after = Species),
  iris[, c("Sepal.Length", "Sepal.Width", "Species", "Petal.Length", "Petal.Width")],
  info = "Test .after works when .after is the last column"
)

expect_error(
  mtcars %>% relocate(gear, .after = mpg, .before = cyl),
  info = "relocate() fails when .after and .before are both given"
)
