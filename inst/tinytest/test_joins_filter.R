table1 <- data.frame(
  id = 1:5,
  animal = c("cat", "dog", "hamster", "parrot", "horse"),
  stringsAsFactors = FALSE
)
table2 <- table1[c(1, 3, 5), ]

expect_equal(
  anti_join(table1, table2, by = "id"),
  data.frame(id = c(2, 4), animal = c("dog", "parrot"), stringsAsFactors = FALSE),
  info = "Single column anti-join"
)

table1 <- data.frame(
  pupil = rep(1:3, each = 2),
  test = rep(c("A", "B"), 3),
  score = c(60, 70, 65, 80, 85, 70),
  stringsAsFactors = FALSE
)
table2 <- table1[c(1, 3, 4), ]
expect_equal(
  anti_join(table1, table2, by = c("pupil", "test")),
  data.frame(pupil = c(1, 3, 3), test = c("B", "A", "B"), score = c(70, 85, 70), stringsAsFactors = FALSE),
  info = "Multi-column anti-join"
)
