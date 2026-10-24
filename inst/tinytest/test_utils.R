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

eg_gd <- group_by(data.frame(g = c("b", "a", "b"), x = c(1, 2, 5)), g)
eg_res <- poorman:::eval_groups(eg_gd, list(m = quote(mean(x)), quote(m * 2), y = quote(x)), environment())
expect_equal(eg_res$rows, list(2L, c(1L, 3L)), info = "eval_groups() returns the rows of each group")
expect_equal(eg_res$labels, c("m", "m * 2", "y"), info = "eval_groups() names unnamed expressions automatically")
expect_equal(
  eg_res$values,
  list(list(2, 3), list(4, 6), list(2, c(1, 5))),
  info = "eval_groups() returns the result of each expression for each group, seeing earlier results"
)
expect_equal(eg_res$sizes, c(1L, 2L), info = "eval_groups() returns the size of the results of each group")
