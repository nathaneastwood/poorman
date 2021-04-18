expect_equal(
  union_all(1:3, 4:6),
  1:6,
  info = "union_all() on vectors concatenates"
)

df1 <- data.frame(x = 1:2)
df2 <- data.frame(y = 1:2)
expect_equal(
  union_all(df1, df2),
  bind_rows(df1, df2),
  info = "union_all() on data frames calls bind rows"
)
