expect_equal(
  mtcars %>% rename(MilesPerGallon = mpg),
  {
    res <- mtcars
    colnames(res)[1] <- "MilesPerGallon"
    res
  },
  info = "Test renaming a single column"
)

expect_warning(
  mtcars %>% rename(),
  info = "Test that a warning is given when no new names are passed to rename()"
)

expect_warning(
  mtcars %>% rename(Cyls = cyl, mpg),
  info = "Test that a warning is given when a column is not provided a new name in rename()"
)

# Grouped Operations
expect_equal(
  mtcars %>% rename(Gears = gear, Auto = am),
  {
    res <- mtcars
    colnames(res)[which(colnames(res) %in% c("am", "gear"))] <- c("Auto", "Gears")
    res
  },
  info = "Test renaming multiple columns"
)

ce_nm <- "tot"
expect_equal(
  names(rename(data.frame(x = 1, y = 2), z := x, "{ce_nm}" := y)), # nolint: object_name_linter.
  c("z", "tot"),
  info = "rename() supports :="
)
