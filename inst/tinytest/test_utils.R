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

expect_true(poorman:::is_all_na_logical(c(NA, NA)), info = "is_all_na_logical() is TRUE for a logical vector of NA")
expect_true(poorman:::is_all_na_logical(c(a = NA)), info = "is_all_na_logical() allows names")
expect_false(poorman:::is_all_na_logical(logical()), info = "is_all_na_logical() is FALSE for an empty vector")
expect_false(
  poorman:::is_all_na_logical(c(NA, TRUE)), info = "is_all_na_logical() is FALSE when a value is not missing"
)
expect_false(poorman:::is_all_na_logical(NA_integer_), info = "is_all_na_logical() is FALSE for other types")
expect_false(poorman:::is_all_na_logical(matrix(NA)), info = "is_all_na_logical() is FALSE for a matrix")
expect_false(
  poorman:::is_all_na_logical(structure(NA, class = "foo")), info = "is_all_na_logical() is FALSE for a classed vector"
)
