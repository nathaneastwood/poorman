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
eg_mut <- poorman:::eval_groups(
  eg_gd, list(m = quote(mean(x)), x = quote(NULL), k = quote(exists("x"))), environment(),
  group_size = TRUE, drop_null = TRUE
)
expect_equal(eg_mut$sizes, c(1L, 2L), info = "eval_groups() with group_size gives the size of each group")
expect_equal(
  eg_mut$values[[3L]],
  list(FALSE, FALSE),
  info = "eval_groups() with drop_null removes a variable whose result is NULL"
)
expect_error(
  poorman:::eval_groups(eg_gd, list(y = quote(1:3)), environment(), group_size = TRUE),
  "must be size 1 or 1, not 3",
  info = "eval_groups() with group_size checks that each result has the size of its group or size one"
)

expect_equal(poorman:::ungroup_order(list(2L, c(1L, 3L)), 3L), c(2L, 1L, 3L), info = "ungroup_order() inverts the rows")
expect_null(poorman:::ungroup_order(list(1:3), 3L), info = "ungroup_order() gives NULL for rows already in order")
expect_null(poorman:::ungroup_order(list(1L, 2:3), 3L), info = "ungroup_order() gives NULL for groups in row order")

expect_equal(
  poorman:::combine_groups(list(1, c(2, 3)), c(2L, 2L), "x"),
  c(1, 1, 2, 3),
  info = "combine_groups() recycles results of size one to the size of their group"
)
expect_null(poorman:::combine_groups(list(NULL, NULL), c(1L, 1L), "x"), info = "combine_groups() ignores NULL results")
expect_error(
  poorman:::combine_groups(list(NULL, 1), c(1L, 1L), "x"),
  "can't combine NULL and non NULL results",
  info = "combine_groups() cannot combine NULL with other results"
)
