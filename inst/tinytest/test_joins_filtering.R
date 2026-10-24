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

nax <- data.frame(a = c(1, NA, NaN, 2, NA), b = c("p", "q", "r", NA, "t"), v = 1:5)
nay <- data.frame(a = c(1, NA, NaN, NA), b = c("p", "q", "r", "t"))

expect_equal(
  list(semi_join(nax, nay, by = "a")$v, anti_join(nax, nay, by = "a")$v),
  list(c(1L, 2L, 3L, 5L), 4L),
  info = "By default, NA matches NA and NaN matches NaN in filtering joins"
)

expect_equal(
  list(semi_join(nax, nay, by = "a", na_matches = "never")$v, anti_join(nax, nay, by = "a", na_matches = "never")$v),
  list(1L, 2:5),
  info = "na_matches = 'never' stops NA and NaN keys matching in filtering joins"
)

expect_equal(
  list(
    semi_join(nax, nay, by = c("a", "b"), na_matches = "never")$v,
    semi_join(nax, nay, by = "b", na_matches = "never")$v
  ),
  list(1L, c(1L, 2L, 3L, 5L)),
  info = "na_matches = 'never' only drops the rows with a missing value in one of the keys"
)

expect_error(
  semi_join(nax, nay, by = "a", na_matches = "no"),
  info = "Filtering joins reject an invalid na_matches"
)

expect_equal(
  semi_join(nax, nay, "a", TRUE, foo = 1),
  semi_join(nax, nay, by = "a"),
  info = "Filtering joins ignore copy and the arguments in ..."
)

expect_equal(
  formals(semi_join),
  formals(anti_join),
  info = "semi_join() and anti_join() have the same arguments"
)
expect_equal(
  names(formals(semi_join)),
  c("x", "y", "by", "copy", "...", "na_matches"),
  info = "Filtering joins have the dplyr arguments"
)

res <- semi_join(group_by(nax, b), nay, by = "a", na_matches = "never")
expect_equal(
  list(res$v, group_vars(res)),
  list(1L, "b"),
  info = "na_matches works on grouped data in filtering joins"
)
