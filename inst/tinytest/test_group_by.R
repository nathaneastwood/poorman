expect_error(
  mtcars %>% group_by(fake_column_name)
)

res <- suppressMessages(mtcars %>% group_by(am, cyl) %>% select(mpg))
expect_equal(
  colnames(res),
  c("am", "cyl", "mpg"),
  info = "Test that groups persist in select() when no groups are selected"
)

res <- suppressMessages(mtcars %>% group_by(am, cyl) %>% select(mpg, cyl))
expect_equal(
  colnames(res),
  c("am", "mpg", "cyl"),
  info = "Test that groups persist in select() when only some groups are selected"
)

res <- mtcars %>% group_by(am, cyl) %>% relocate(gear, .before = mpg)
expect_equal(
  colnames(res),
  c("gear", "mpg", "cyl", "disp", "hp", "drat", "wt", "qsec", "vs", "am", "carb"),
  info = "Test that groups persist in relocate()"
)

res <- mtcars %>% group_by(am, cyl) %>% rename(Gears = gear)
expect_equal(
  colnames(res),
  c("mpg", "cyl", "disp", "hp", "drat", "wt", "qsec", "vs", "am", "Gears", "carb"),
  info = "Test that groups persist in rename()"
)

res <- mtcars %>% group_by(tmp = am * cyl)
expect_equal(
  colnames(res),
  c("mpg", "cyl", "disp", "hp", "drat", "wt", "qsec", "vs", "am", "gear", "carb", "tmp"),
  info = "group_by() can create new columns #1"
)
expect_equal(
  res$tmp,
  mtcars$am * mtcars$cyl,
  info = "group_by() can create new columns #2"
)

rm(res)
