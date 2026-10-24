table1 <- data.frame(
  id = 1:5,
  animal = c("cat", "dog", "hamster", "parrot", "horse"),
  stringsAsFactors = FALSE
)
table2 <- table1[c(1, 3, 5), ]

expect_equal(
  {
    invisible(suppressMessages(res <- table1 %>% anti_join(table2)))
    res
  },
  data.frame(id = c(2, 4), animal = c("dog", "parrot"), stringsAsFactors = FALSE),
  info = "Single column anti-join"
)

expect_equal(
  {
    invisible(suppressMessages(res <- table1 %>% semi_join(table2)))
    res
  },
  data.frame(id = c(1L, 3L, 5L), animal = c("cat", "hamster", "horse"), stringsAsFactors = FALSE),
  info = "Single column semi-join"
)

table1 <- data.frame(
  pupil = rep(1:3, each = 2),
  test = rep(c("A", "B"), 3),
  score = c(60, 70, 65, 80, 85, 70),
  stringsAsFactors = FALSE
)
table2 <- table1[c(1, 3, 4), ]

expect_equal(
  table1 %>% anti_join(table2, by = c("pupil", "test")),
  data.frame(pupil = c(1, 3, 3), test = c("B", "A", "B"), score = c(70, 85, 70), stringsAsFactors = FALSE),
  info = "Multi-column anti-join"
)

expect_equal(
  table1 %>% semi_join(table2, by = c("pupil", "test")),
  data.frame(pupil = c(1L, 2L, 2L), test = c("A", "A", "B"), score = c(60, 65, 80), stringsAsFactors = FALSE),
  info = "Multi-column semi-join"
)

tbl1 <- data.frame(
  a = rep(LETTERS[1:3], 10),
  stringsAsFactors = FALSE
)
tbl2 <- data.frame(
  a = LETTERS[1:3],
  b = colours()[1:3],
  stringsAsFactors = FALSE
)
expect_equal(
  invisible(suppressMessages(res <- semi_join(tbl1, tbl2))),
  data.frame(
    a = rep(LETTERS[1:3], 10),
    stringsAsFactors = FALSE
  ),
  info = "Ensure `drop = FALSE` for single column data.frames."
)

df1 <- data.frame(x = "a.b", y = "c", stringsAsFactors = FALSE)
df2 <- data.frame(x = "a", y = "b.c", stringsAsFactors = FALSE)
expect_equal(
  nrow(semi_join(df1, df2, by = c("x", "y"))),
  0L,
  info = "semi_join() does not match keys whose pasted values are the same"
)
expect_equal(
  nrow(anti_join(df1, df2, by = c("x", "y"))),
  1L,
  info = "anti_join() does not match keys whose pasted values are the same"
)
rm(df1, df2)

x <- data.frame(k = 1:3, v = c("a", "b", "c"), stringsAsFactors = FALSE)
y <- data.frame(m = 2:4)
expect_equal(
  semi_join(x, y, by = c(k = "m")),
  data.frame(k = 2:3, v = c("b", "c"), stringsAsFactors = FALSE),
  info = "semi_join() joins different columns with a named `by`"
)
expect_equal(
  anti_join(x, y, by = c(k = "m")),
  data.frame(k = 1L, v = "a", stringsAsFactors = FALSE),
  info = "anti_join() joins different columns with a named `by`"
)
expect_identical(
  semi_join(x, data.frame(k = c(2, 3)), by = "k")$k, 2:3, info = "Filtering joins keep the key type of x"
)
expect_identical(
  semi_join(data.frame(k = factor(c("p", "q"))), data.frame(k = "q", stringsAsFactors = FALSE), by = "k")$k,
  factor("q", levels = c("p", "q")),
  info = "Filtering joins keep factor keys of x"
)
expect_error(
  semi_join(x, data.frame(k = c("1", "2")), by = "k"),
  "Can't combine `k` <integer> and `k` <character>.",
  fixed = TRUE,
  info = "Filtering joins error on keys of incompatible types"
)
expect_error(
  anti_join(x, y, by = "nope"),
  "Join columns must be present in data",
  info = "Filtering joins error on missing join columns"
)
expect_error(
  semi_join(x, data.frame(q = 1)),
  "`by` must be supplied when `x` and `y` have no common variables",
  info = "Natural filtering joins error when there are no common columns"
)
rm(x, y)
