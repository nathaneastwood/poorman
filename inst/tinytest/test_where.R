expect_error(
  iris %>% select(is.numeric),
  info = "Predicate functions must be wrapped in `where()`"
)

df <- data.frame(
  is.col1 = 1:5, is.col2 = letters[1:5],
  check.names = FALSE,
  stringsAsFactors = FALSE
)
expect_equal(
  df %>% select(is.col1),
  df[, "is.col1", drop = FALSE],
  info = "Columns starting with is. are still selected"
)

expect_error(
  df %>% select(where(is.blah)),
  info = "Non-existent predicates are found out"
)

expect_equal(
  df %>% select(where(is.character)),
  df[, "is.col2", drop = FALSE],
  info = "The user can select columns using predicates"
)

df <- data.frame(a = 1:10, b = 101:110)
expect_equal(
  df %>% select(where(function(x) mean(x) > 10)),
  df[, "b", drop = FALSE],
  info = "Custom functions can be used in where()"
)

expect_error(
  df %>% select(where(function(x) if (mean(x) > 10) "yes")),
  info = "where() must be used with functions that return `TRUE` or `FALSE`"
)

rm(df)

df <- data.frame(a = 1:3, b = c("x", "y", "z"), d = c(10, 20, 30))
expect_equal(
  select(df, where(~ is.numeric(.x))),
  df[, c("a", "d")],
  info = "where() accepts a formula"
)
expect_equal(
  select(df, where(~ is.numeric(.) && mean(.) > 5)),
  df[, "d", drop = FALSE],
  info = "where() accepts a formula using the . pronoun"
)
expect_equal(
  select(group_by(df, a), where(~ is.character(.x))),
  group_by(df[, c("a", "b")], a),
  info = "where() accepts a formula in grouped data"
)
expect_error(select(df, where(y ~ is.numeric(.x))), info = "where() rejects a two-sided formula")
expect_error(select(df, where(1)), info = "where() rejects an object which is not a function or a formula")
rm(df)
