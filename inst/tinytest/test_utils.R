expect_error(
  1L %>% select("mpg"),
  info = "Test non-data.frame is not a data.frame"
)

expect_equal(
  poorman:::deparse_var("val"),
  "val",
  info = "deparse_var() works for character strings"
)

expect_equal(
  poorman:::deparse_var(val),
  "val",
  info = "deparse_var() works for symbols"
)

expect_equal(
  poorman:::deparse_var(NULL),
  NULL,
  info = "deparse_var() works for NULL values"
)
