expect_equal(
  sapply(iris, n_distinct),
  sapply(iris, function(.) length(unique(.))),
  info = "n_distinct() gives the correct results on a data.frame"
)

df_var <- data.frame(
  l = c(TRUE, FALSE, FALSE),
  i = c(1, 1, 2),
  d = Sys.Date() + c(1, 1, 2),
  f = factor(letters[c(1, 1, 2)]),
  n = c(1, 1, 2) + 0.5,
  t = Sys.time() + c(1, 1, 2),
  c = letters[c(1, 1, 2)],
  stringsAsFactors = FALSE
)
expect_equal(
  sapply(df_var, n_distinct),
  sapply(df_var, function(.) length(unique(.))),
  info = "n_distinct() gives correct results for key types"
)

expect_equal(
  n_distinct(c(1.0, NA, NA)),
  2,
  info = "n_distinct() treats NA correctly"
)

expect_equal(
  n_distinct(1, 1:4),
  4,
  info = "n_distinct() recycles length 1 vectors"
)
expect_equal(
  n_distinct(1:4, 1),
  4,
  info = "n_distinct() recycles length 1 vectors"
)

x <- iris$Sepal.Length
y <- iris$Sepal.Width
expect_equal(
  n_distinct(iris$Sepal.Length, iris$Sepal.Width),
  n_distinct(x, y),
  info = "n_distinct() handles unnamed"
)

expect_identical(
  data.frame(x = 42) %>% summarise(n = n_distinct(.data$x)),
  data.frame(n = 1L),
  info = "n_distinct() respects .data"
)

rm(df_var, x, y)
