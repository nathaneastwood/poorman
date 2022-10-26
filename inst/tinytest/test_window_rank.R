expect_equal(
  cume_dist(c(1:3, NA)),
  c(1 / 3, 2 / 3, 1, NA),
  info = "cume_dist() ignores NAs"
)

expect_equal(
  ntile(c(1:3, NA, NA, NA), 3),
  c(1:3, NA, NA, NA),
  info = "ntile() ignores number of NAs"
)
expect_equal(
  ntile(c(1:3, NA, NA, NA), 1),
  c(1L, 1L, 1L, NA, NA, NA),
  info = "ntile() ignores number of NAs"
)

expect_equal(
  ntile(numeric(), 3),
  integer(),
  info = "ntile() always returns an integer"
)
expect_equal(
  ntile(NA, 3),
  NA_integer_,
  info = "ntile() always returns an integer"
)

expect_equal(ntile(1, 5), 1, info = "ntile() puts large groups first")
expect_equal(ntile(1:2, 5), 1:2, info = "ntile() puts large groups first")
expect_equal(ntile(1:3, 5), 1:3, info = "ntile() puts large groups first")
expect_equal(ntile(1:4, 5), 1:4, info = "ntile() puts large groups first")
expect_equal(ntile(1:5, 5), 1:5, info = "ntile() puts large groups first")
expect_equal(ntile(1:6, 5), c(1, 1:5), info = "ntile() puts large groups first")
expect_equal(ntile(1, 7), 1, info = "ntile() puts large groups first")
expect_equal(ntile(1:2, 7), 1:2, info = "ntile() puts large groups first")
expect_equal(ntile(1:3, 7), 1:3, info = "ntile() puts large groups first")
expect_equal(ntile(1:4, 7), 1:4, info = "ntile() puts large groups first")
expect_equal(ntile(1:5, 7), 1:5, info = "ntile() puts large groups first")
expect_equal(ntile(1:6, 7), 1:6, info = "ntile() puts large groups first")
expect_equal(ntile(1:7, 7), 1:7, info = "ntile() puts large groups first")
expect_equal(ntile(1:8, 7), c(1, 1:7), info = "ntile() puts large groups first")

expect_equal(
  percent_rank(c(1:3, NA)),
  c(0, 0.5, 1, NA),
  info = "percent_rank() ignores NAs"
)

df <- data.frame(
  x = 1:10, y = seq(1, 10, by = 1),
  g = rep(c(1, 2), each = 5), s = c(letters[1:3], LETTERS[1:5], letters[4:5])
)
expect_equal(
  mutate(df, rank = min_rank(desc(x)))$rank,
  10:1,
  info = "desc() is correctly handled by window functions"
)
expect_equal(
  mutate(group_by(df, g), rank = min_rank(desc(x)))$rank,
  rep(5:1, 2),
  info = "desc() is correctly handled by window functions"
)
expect_equal(
  mutate(df, rank = row_number(desc(x)))$rank,
  10:1,
  info = "desc() is correctly handled by window functions"
)
expect_equal(
  mutate(group_by(df, g), rank = row_number(desc(x)))$rank,
  rep(5:1, 2),
  info = "desc() is correctly handled by window functions"
)

expect_equal(
  {
    data.frame(
      id = rep(c(1, 2), each = 5), value = c(1, 1, 2, 5, 0, 6, 4, 0, 0, 2),
      s = c(letters[1:2], LETTERS[1:4], letters[2:5])
    ) %>%
      group_by(id) %>%
      mutate(var = row_number(value)) %>%
      pull(var)
  },
  c(2, 3, 4, 5, 1, 5, 4, 1, 2, 3),
  info = "row_number() gives correct results"
)

g <- group_by(mtcars, cyl)
expect_equal(
  mutate(g, rn = row_number()),
  mutate(g, rn = 1:n()),
  info = "row_number() works with 0 arguments"
)

expect_true(
  {
    test <- data.frame(
      Name = c("a", "b", "c", "d", "e"),
      ID = c(1, 1, 1, 1, 1),
      expression = c(NaN, NaN, NaN, NaN, NaN)
    ) %>%
      group_by(ID) %>%
      mutate(rank = min_rank(expression))
    all(is.na(test$rank))
  },
  info = "min_rank() handles columns full of NaN"
)

df <- data.frame(x = 1:42)
expect_identical(
  mutate(df, nt = ntile(n = 9)),
  mutate(df, nt = ntile(row_number(), n = 9)),
  info = "ntile() works with one argument"
)
df <- group_by(data.frame(x = 1:42, g = rep(1:7, each = 6)), g)
expect_identical(
  mutate(df, nt = ntile(n = 4)),
  mutate(df, nt = ntile(row_number(), n = 4)),
  info = "ntile() works with one argument"
)

data <- data.frame(x = c(1, 2, NA, 1, 0, NA))
res <- data %>%
  mutate(
    min_rank = min_rank(x),
    percent_rank = percent_rank(x),
    dense_rank = dense_rank(x),
    cume_dist = cume_dist(x),
    ntile = ntile(x, 2),
    row_number = row_number(x)
  )
expect_true(all(is.na(res$min_rank[c(3, 6)])), info = "min_rank() deals correctly with NA")
expect_equal(
  res$min_rank[c(1, 2, 4, 5)],
  c(2L, 4L, 2L, 1L),
  info = "min_rank() deals correctly with NA"
)
expect_true(all(is.na(res$dense_rank[c(3, 6)])), info = "dense_rank() deals correctly with NA")
expect_equal(
  res$dense_rank[c(1, 2, 4, 5)],
  c(2L, 3L, 2L, 1L),
  info = "dense_rank() deals correctly with NA"
)
expect_true(all(is.na(res$percent_rank[c(3, 6)])), info = "percent_rank() deals correctly with NA")
expect_equal(
  res$percent_rank[c(1, 2, 4, 5)],
  c(1 / 3, 1, 1 / 3, 0),
  info = "percent_rank() deals correctly with NA"
)
expect_true(all(is.na(res$cume_dist[c(3, 6)])), info = "cume_dist() deals correctly with NA")
expect_equal(
  res$cume_dist[c(1, 2, 4, 5)],
  c(.75, 1, .75, .25),
  info = "cume_dist() deals correctly with NA"
)
expect_true(all(is.na(res$ntile[c(3, 6)])), info = "ntile() deals correctly with NA")
expect_equal(
  res$ntile[c(1, 2, 4, 5)],
  c(1L, 2L, 2L, 1L),
  info = "ntile() deals correctly with NA"
)
expect_true(all(is.na(res$row_number[c(3, 6)])), info = "row_number() deals correctly with NA")
expect_equal(
  res$row_number[c(1, 2, 4, 5)],
  c(2L, 4L, 3L, 1L),
  info = "row_number() deals correctly with NA"
)
data <- data.frame(
  x = rep(c(1, 2, NA, 1, 0, NA), 2),
  g = rep(c(1, 2), each = 6)
)
res <- data %>%
  group_by(g) %>%
  mutate(
    min_rank = min_rank(x),
    percent_rank = percent_rank(x),
    dense_rank = dense_rank(x),
    cume_dist = cume_dist(x),
    ntile = ntile(x, 2),
    row_number = row_number(x)
  )
expect_true(all(is.na(res$min_rank[c(3, 6, 9, 12)])), info = "min_rank() deals correctly with NA")
expect_equal(
  res$min_rank[c(1, 2, 4, 5, 7, 8, 10, 11)],
  rep(c(2L, 4L, 2L, 1L), 2),
  info = "min_rank() deals correctly with NA"
)
expect_true(all(is.na(res$dense_rank[c(3, 6, 9, 12)])), info = "dense_rank() deals correctly with NA")
expect_equal(
  res$dense_rank[c(1, 2, 4, 5, 7, 8, 10, 11)],
  rep(c(2L, 3L, 2L, 1L), 2),
  info = "dense_rank() deals correctly with NA"
)
expect_true(all(is.na(res$percent_rank[c(3, 6, 9, 12)])), info = "percent_rank() deals correctly with NA")
expect_equal(
  res$percent_rank[c(1, 2, 4, 5, 7, 8, 10, 11)],
  rep(c(1 / 3, 1, 1 / 3, 0), 2),
  info = "percent_rank() deals correctly with NA"
)
expect_true(all(is.na(res$cume_dist[c(3, 6, 9, 12)])), info = "cume_dist() deals correctly with NA")
expect_equal(
  res$cume_dist[c(1, 2, 4, 5, 7, 8, 10, 11)],
  rep(c(.75, 1, .75, .25), 2),
  info = "cume_dist() deals correctly with NA"
)
expect_true(all(is.na(res$ntile[c(3, 6, 9, 12)])), info = "ntile() deals correctly with NA")
expect_equal(
  res$ntile[c(1, 2, 4, 5, 7, 8, 10, 11)],
  rep(c(1L, 2L, 2L, 1L), 2),
  info = "ntile() deals correctly with NA"
)
expect_true(all(is.na(res$row_number[c(3, 6, 9, 12)])), info = "row_number() deals correctly with NA")
expect_equal(
  res$row_number[c(1, 2, 4, 5, 7, 8, 10, 11)],
  rep(c(2L, 4L, 3L, 1L), 2),
  info = "row_number() deals correctly with NA"
)

rm(data, df, g, res, test)
