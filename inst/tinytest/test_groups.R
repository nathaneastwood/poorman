expect_error(
  mtcars %>% group_by(fake_column_name)
)
