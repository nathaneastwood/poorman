expect_equal(
  mtcars %>% rename(MilesPerGallon = mpg),
  {
    res <- mtcars
    colnames(res)[1] <- "MilesPerGallon"
    res
  },
  info = "Test renaming a single column"
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
