expect_error(
  1L %>% select("mpg"),
  info = "Test non-data.frame is not a data.frame"
)
