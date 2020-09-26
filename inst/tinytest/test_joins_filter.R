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
