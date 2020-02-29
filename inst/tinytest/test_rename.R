expect_equal(
  mtcars %>% rename(MilesPerGallon = mpg),
  {
    colnames(mtcars)[1] <- "MilesPerGallon"
    mtcars
  },
  info = "Test renaming a single column"
)

# Grouped Operations
expect_equal(
  mtcars %>% rename(Gears = gear, Auto = am),
  {
    colnames(mtcars)[which(colnames(mtcars) %in% c("gear", "am"))] <- c("Gears", "Auto")
    mtcars
  },
  info = "Test renaming multiple columns"
)
