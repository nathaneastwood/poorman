expect_equal(
  mtcars %>% slice(),
  mtcars,
  info = "Empty slice() returns the input data"
)

expect_equal(
  mtcars %>% slice(1, 5, 9),
  mtcars[c(1, 5, 9), ],
  info = "Test that slice() handles numeric input"
)

expect_equal(
  mtcars %>% group_by(am, cyl) %>% slice(1, 2) %>% ungroup(),
  do.call(rbind, unname(lapply(split(mtcars, list(mtcars$cyl, mtcars$am)), function(x) x[1:2, ]))),
  info = "Test that grouped slice() operations handle numeric input"
)

expect_equal(
  mtcars %>% slice(-(1:2)),
  mtcars[-(1:2), ],
  info = "Test that slice() works with negative indices"
)

expect_error(
  mtcars %>% slice(-1, 1),
  info = "`slice()` expressions should return either all positive or all negative."
)

expect_error(
  mtcars %>% slice(TRUE),
  info = "`slice()` expressions should return indices (positive or negative integers)."
)

expect_equal(
  mtcars %>% slice(1:3),
  mtcars[1:3, ],
  info = "Test slicing rows with sequence"
)

expect_equal(
  mtcars %>% slice(1:100),
  mtcars,
  info = "Test that extra rows aren't returned if the user slices more rows than are available"
)

expect_equal(
  mtcars %>% slice(2, 2, 2),
  mtcars[c(2, 2, 2), ],
  info = "Test that slicing the same row number n times duplicates that row n times"
)

expect_equal(
  mtcars %>% slice(NA),
  mtcars[0, ],
  info = "Test that slice() handles NA values"
)

expect_equal(
  mtcars %>% slice(1, NA),
  mtcars[1, ],
  info = "Test that slice() handles a mixture of NA and numeric values"
)

expect_equal(
  data.frame(x = numeric()) %>% slice(1:3),
  structure(list(x = numeric(0)), row.names = integer(0), class = "data.frame"),
  info = "Test that slice() handles empty data.frames"
)

expect_equal(
  data.frame(a = 1:3) %>% select(a) %>% slice(1) %>% nrow(),
  1L,
  info = "Test that slice() handles 0-column data.frames"
)

# Slice variants

df <- data.frame(x = 1:5)
expect_equal(df %>% slice_head(n = 6) %>% nrow(), 5, info = "slice_head() silently truncates results")
expect_equal(df %>% slice_tail(n = 6) %>% nrow(), 5, info = "slice_tail() silently truncates results")
expect_equal(df %>% slice_sample(n = 6) %>% nrow(), 5, info = "slice_sample() silently truncates results")
expect_equal(df %>% slice_min(x, n = 6) %>% nrow(), 5, info = "slice_min() silently truncates results")
expect_equal(df %>% slice_max(x, n = 6) %>% nrow(), 5, info = "slice_max() silently truncates results")

df <- data.frame(x = 1:10)
expect_equal(df %>% slice_head(prop = 0.11) %>% nrow(), 1, info = "slice_head() correctly computes the proportion")
expect_equal(df %>% slice_tail(prop = 0.11) %>% nrow(), 1, info = "slice_tail() correctly computes the proportion")
expect_equal(df %>% slice_sample(prop = 0.11) %>% nrow(), 1, info = "slice_sample() correctly computes the proportion")
expect_equal(df %>% slice_min(x, prop = 0.11) %>% nrow(), 1, info = "slice_min() correctly computes the proportion")
expect_equal(df %>% slice_max(x, prop = 0.11) %>% nrow(), 1, info = "slice_max() correctly computes the proportion")
expect_equal(
  df %>% slice_min(x, prop = 0.11, with_ties = FALSE) %>% nrow(),
  1,
  info = "slice_min(with_ties = FALSE) correctly computes the proportion"
)
expect_equal(
  df %>% slice_max(x, prop = 0.11, with_ties = FALSE) %>% nrow(),
  1,
  info = "slice_max(with_ties = FALSE) correctly computes the proportion"
)

df <- data.frame(x = c(1, 1, 1, 2, 2))
expect_equal(df %>% slice_min(x) %>% nrow(), 3, info = "slice_min() returns ties by default")
expect_equal(df %>% slice_max(x) %>% nrow(), 2, info = "slice_max() returns ties by default")
expect_equal(
  df %>% slice_min(x, with_ties = FALSE) %>% nrow(),
  1,
  info = "slice_min(with_ties = FALSE) does not return ties"
)
expect_equal(
  df %>% slice_max(x, with_ties = FALSE) %>% nrow(),
  1,
  info = "slice_max(with_ties = FALSE) does not return ties"
)

df <- data.frame(id = 1:4, x = c(2, 3, 1, 2))
expect_equal(df %>% slice_min(x, n = 2) %>% pull(id), c(3, 1, 4), info = "slice_min() reorders results")
expect_equal(
  df %>% slice_min(x, n = 2, with_ties = FALSE) %>% pull(id),
  c(3, 1),
  info = "slice_min(with_ties = FALSE) reorders results"
)
expect_equal(df %>% slice_max(x, n = 2) %>% pull(id), c(2, 1, 4), info = "slice_max() reorders results")
expect_equal(
  df %>% slice_max(x, n = 2, with_ties = FALSE) %>% pull(id),
  c(2, 1),
  info = "slice_max(with_ties = FALSE) reorders results"
)

df <- data.frame(id = 1:4, x = c(2, NA, 1, 2), y = c(NA, NA, NA, NA))
expect_equal(df %>% slice_min(x, n = 2) %>% pull(id), c(3, 1, 4), info = "slice_min() ignores NAs")
expect_equal(df %>% slice_min(y, n = 2) %>% nrow(), 0, info = "slice_min() ignores NAs")
expect_equal(df %>% slice_max(x, n = 2) %>% pull(id), c(1, 4), info = "slice_max() ignores NAs")
expect_equal(df %>% slice_max(y, n = 2) %>% nrow(), 0, info = "slice_max() ignores NAs")

df <- data.frame(x = 1:100, wt = c(1, rep(0, 99)))
expect_equal(
  df %>% slice_sample(n = 1, weight_by = wt) %>% pull(x),
  1,
  info = "Arguments to slice_sample() are passed along"
)
expect_equal(
  df %>% slice_sample(n = 2, weight_by = wt, replace = TRUE) %>% pull(x),
  c(1, 1),
  info = "Arguments to slice_sample() are passed along"
)

expect_error(
  poorman:::check_slice_size(n = c(1, 2)), "`n` must be a single number.",
  info = "check_slice_size() rejects an n of length 2"
)
expect_error(
  poorman:::check_slice_size(n = NA), "`n` must be a single number.",
  info = "check_slice_size() rejects a missing n"
)
expect_error(
  poorman:::check_slice_size(prop = c(1, 2)), "`prop` must be a single number.",
  info = "check_slice_size() rejects a prop of length 2"
)
expect_error(
  poorman:::check_slice_size(prop = NA), "`prop` must be a single number.",
  info = "check_slice_size() rejects a missing prop"
)
expect_error(
  poorman:::check_slice_size(n = 1, prop = 0.1), "Must supply `n` or `prop`, but not both.",
  info = "check_slice_size() rejects both n and prop"
)

rm(df)

# Grouped slice helpers pass on only the size argument which was supplied
df <- data.frame(g = c(1, 1, 1, 2, 2), v = c(3, 1, 2, 5, 4))
gf <- group_by(df, g)
expect_equal(slice_head(gf, n = 1)$v, c(3, 5), info = "grouped slice_head() with n")
expect_equal(slice_head(gf)$v, c(3, 5), info = "grouped slice_head() without n or prop")
expect_equal(slice_head(gf, prop = 0.5)$v, c(3, 5), info = "grouped slice_head() with prop")
expect_equal(slice_tail(gf, n = 2)$v, c(1, 2, 5, 4), info = "grouped slice_tail() with n")
expect_equal(slice_min(gf, v, n = 1)$v, c(1, 4), info = "grouped slice_min() with n")
expect_equal(slice_max(gf, v, prop = 0.5)$v, c(3, 5), info = "grouped slice_max() with prop")
expect_equal(nrow(slice_sample(gf, n = 1)), 2L, info = "grouped slice_sample() with n")
expect_error(slice_head(gf, n = 1, prop = 0.5), "but not both", info = "grouped slice_head() rejects n and prop")
expect_error(slice_min(gf), "order_by", info = "grouped slice_min() requires order_by")
rm(df, gf)

# Grouped slice() evaluates its indices for each group through the shared engine
df <- data.frame(g = c(2, 1, 2, 1, 2), v = c(5, 3, 1, 4, 2), w = c(1, 1, 1, 1, 10))
rownames(df) <- c("a", "b", "c", "d", "e")
gf <- group_by(df, g)
res <- slice(gf, 1)
expect_equal(res$v, c(3, 5), info = "grouped slice() returns the rows in group order")
expect_equal(rownames(res), c("b", "a"), info = "grouped slice() keeps the row names of the input")
expect_equal(group_data(res)$.rows, list(1L, 2L), info = "grouped slice() recalculates the group rows")
expect_identical(slice(gf), gf, info = "grouped slice() without indices returns the input in its order")
expect_equal(slice(gf, -1)$v, c(4, 1, 2), info = "grouped slice() drops rows with negative indices")
expect_equal(slice(gf, n())$v, c(4, 2), info = "grouped slice() can use n()")
expect_equal(
  slice(gf, if (cur_group_id() == 1L) 1L else 2:3)$v, c(3, 1, 2),
  info = "grouped slice() can use cur_group_id()"
)
res <- slice(gf, 3)
expect_equal(res$v, 2, info = "grouped slice() ignores indices beyond the size of the group")
expect_equal(group_data(res)$g, 2, info = "grouped slice() drops groups left empty")
res <- slice(gf, c(1, 1))
expect_equal(res$v, c(3, 3, 5, 5), info = "grouped slice() can repeat a row")
expect_equal(rownames(res), c("b", "b.1", "a", "a.1"), info = "grouped slice() makes repeated row names unique")
expect_equal(group_data(res)$.rows, list(1:2, 3:4), info = "grouped slice() with repeated rows has the right groups")
f <- function() {
  k <- 2
  slice(gf, k)
}
expect_equal(f()$v, c(4, 1), info = "grouped slice() finds local variables of the caller")
fd <- group_by(data.frame(g = factor(c("x", "x"), levels = c("x", "y")), v = 1:2), g, .drop = FALSE)
res <- slice(fd, 1)
expect_equal(res$v, 1L, info = "grouped slice() with .drop = FALSE")
expect_equal(group_data(res)$.rows, list(1L, integer()), info = "grouped slice() keeps empty groups with .drop = FALSE")
expect_equal(nrow(slice(group_by(df[0, ], g), 1)), 0L, info = "grouped slice() of zero rows")
expect_error(slice(gf, c(-1, 1)), "all positive or all negative", info = "grouped slice() rejects mixed indices")

# slice_min(), slice_max() and slice_sample() evaluate their order and weights as expressions
expect_equal(slice_min(gf, v * w)$v, c(3, 1), info = "grouped slice_min() orders by an expression")
expect_equal(slice_max(gf, v * w, n = 2)$v, c(4, 3, 2, 5), info = "grouped slice_max() orders by an expression")
expect_equal(slice_min(df, v * w)$v, 1, info = "slice_min() orders by an expression")
expect_equal(slice_max(df, v * w)$v, 2, info = "slice_max() orders by an expression")
f <- function() {
  m <- -1
  slice_min(gf, v * m)
}
expect_equal(f()$v, c(4, 5), info = "grouped slice_min() finds local variables of the caller")
f <- function() {
  z <- c(5, 4, 3, 2, 1)
  slice_min(df, z)
}
expect_equal(f()$v, 2, info = "slice_min() can order by a local vector of the size of the data")
expect_error(
  slice_min(gf, c(5, 4, 3, 2, 1)), "`order_by` must have size 2, not size 5.",
  info = "grouped slice_min() checks the size of order_by"
)
expect_equal(
  slice_max(fd, v)$v, 2L,
  info = "grouped slice_max() with an empty group"
)
f <- function() {
  ww <- c(0, 0, 0, 0, 1)
  slice_sample(df, n = 1, weight_by = ww)
}
expect_equal(f()$v, 2, info = "slice_sample() finds a local weight vector")
expect_equal(slice_sample(df, n = 1, weight_by = w == 10)$v, 2, info = "slice_sample() weights by an expression")
expect_equal(
  slice_sample(gf, n = 1, weight_by = v == max(v))$v, c(4, 5),
  info = "grouped slice_sample() evaluates the weights for each group"
)
expect_error(
  slice_sample(gf, n = 1, weight_by = c(0, 1, 0, 1, 0)), "`weight_by` must have size 2, not size 5.",
  info = "grouped slice_sample() checks the size of weight_by"
)
rm(df, gf, fd, f, res)

# A negative n or prop drops that many rows from each group, as in dplyr 1.0.10
df <- data.frame(g = c(1, 1, 1, 1, 2, 2, 3), x = c(3, 1, 1, 4, 5, 2, 9), id = 1:7)
gf <- group_by(df, g)
expect_equal(slice_head(df, n = -1)$id, 1:6, info = "slice_head() with a negative n drops the last rows")
expect_equal(slice_head(gf, n = -1)$id, c(1L, 2L, 3L, 5L), info = "grouped slice_head() with a negative n")
expect_equal(slice_tail(df, n = -2)$id, 3:7, info = "slice_tail() with a negative n drops the first rows")
expect_equal(slice_tail(gf, n = -1)$id, c(2L, 3L, 4L, 6L), info = "grouped slice_tail() with a negative n")
expect_equal(slice_head(gf, n = -2)$id, c(1L, 2L), info = "A negative n drops whole groups no larger than it")
expect_equal(nrow(slice_tail(gf, n = -10)), 0L, info = "A negative n larger than every group drops every row")
expect_equal(nrow(slice_head(df, n = -Inf)), 0L, info = "n = -Inf drops every row")
expect_equal(slice_head(gf, n = -1.5)$id, c(1L, 2L, 3L, 5L), info = "The fraction of a negative n is ignored")
expect_equal(slice_tail(gf, n = -2.7)$id, c(3L, 4L), info = "The fraction of a negative n is ignored by slice_tail()")
expect_equal(slice_head(gf, n = -0.5)$id, 1:7, info = "A negative n above -1 drops no rows")
expect_equal(slice_head(gf, n = -1L)$id, c(1L, 2L, 3L, 5L), info = "A negative integer n")
expect_equal(group_vars(slice_head(gf, n = -1)), "g", info = "A negative n keeps the groups")
expect_equal(
  slice_min(df, x, n = -1)$id, c(2L, 3L, 6L, 1L, 4L, 5L),
  info = "slice_min() with a negative n drops the largest rows"
)
expect_equal(
  slice_max(df, x, n = -1)$id, c(7L, 5L, 4L, 1L, 6L, 2L, 3L),
  info = "slice_max() with a negative n keeps rows tied with the last row kept"
)
expect_equal(slice_min(gf, x, n = -1)$id, c(2L, 3L, 1L, 6L), info = "grouped slice_min() with a negative n")
expect_equal(slice_max(gf, x, n = -1)$id, c(4L, 1L, 2L, 3L, 5L), info = "grouped slice_max() keeps ties")
expect_equal(
  slice_max(gf, x, n = -1, with_ties = FALSE)$id, c(4L, 1L, 2L, 5L),
  info = "grouped slice_max() with a negative n and with_ties = FALSE"
)
expect_equal(slice_min(gf, x, n = -3)$id, c(2L, 3L), info = "grouped slice_min() keeps ties with a negative n")
expect_equal(slice_head(df, prop = -0.6)$id, 1:3, info = "A negative prop drops a proportion of the rows, rounded down")
expect_equal(slice_head(gf, prop = -0.5)$id, c(1L, 2L, 5L, 7L), info = "grouped slice_head() with a negative prop")
expect_equal(slice_tail(gf, prop = -0.25)$id, 2:7, info = "grouped slice_tail() with a negative prop")
expect_equal(slice_tail(gf, prop = -0.75)$id, c(4L, 6L, 7L), info = "grouped slice_tail() with prop = -0.75")
expect_equal(slice_min(gf, x, prop = -0.75)$id, c(2L, 3L, 6L, 7L), info = "grouped slice_min() keeps ties with a prop")
expect_equal(
  slice_min(gf, x, prop = -0.75, with_ties = FALSE)$id, c(2L, 6L, 7L),
  info = "grouped slice_min() with a negative prop and with_ties = FALSE"
)
expect_equal(slice_max(gf, x, prop = -0.5)$id, c(4L, 1L, 5L, 7L), info = "grouped slice_max() with a negative prop")
expect_equal(nrow(slice_head(gf, prop = -1)), 0L, info = "prop = -1 drops every row")
expect_equal(nrow(slice_tail(gf, prop = -2)), 0L, info = "A prop below -1 drops every row")
expect_error(slice_sample(gf, n = -1), "`n` must be positive.", info = "slice_sample() rejects a negative n")
expect_error(slice_sample(df, n = -0.5), "`n` must be positive.", info = "slice_sample() rejects n = -0.5")
expect_error(slice_sample(gf, prop = -0.5), "`prop` must be positive.", info = "slice_sample() rejects a negative prop")
expect_error(slice_sample(df, n = NA), "`n` must be a single number.", info = "slice_sample() checks n is a number")
expect_error(
  slice_tail(df, n = -1, prop = 1), "Must supply `n` or `prop`, but not both.",
  info = "Giving both n and prop is an error, whatever their values"
)
expect_error(slice_head(df, n = "a"), "`n` must be a single number.", info = "A character n is an error")
expect_error(slice_head(df, n = TRUE), "`n` must be a single number.", info = "A logical n is an error")
rm(df, gf)

# A size of zero, or a negative size larger than the group, selects no rows. These differ from dplyr 1.0.10, which
# returns rows for them (or gives an error in slice_sample()); later versions of dplyr select no rows too
df <- data.frame(g = c(1, 1, 1, 1, 2, 2, 3), x = c(3, 1, 1, 4, 5, 2, 9), id = 1:7)
gf <- group_by(df, g)
for (d in list(df, gf)) {
  expect_equal(nrow(slice_head(d, n = 0)), 0L, info = "slice_head() with n = 0 selects no rows")
  expect_equal(nrow(slice_tail(d, n = 0)), 0L, info = "slice_tail() with n = 0 selects no rows")
  expect_equal(nrow(slice_min(d, x, n = 0)), 0L, info = "slice_min() with n = 0 selects no rows")
  expect_equal(nrow(slice_max(d, x, n = 0)), 0L, info = "slice_max() with n = 0 selects no rows")
  expect_equal(nrow(slice_head(d, prop = 0)), 0L, info = "slice_head() with prop = 0 selects no rows")
  expect_equal(nrow(slice_tail(d, prop = 0)), 0L, info = "slice_tail() with prop = 0 selects no rows")
  expect_equal(nrow(slice_min(d, x, prop = 0)), 0L, info = "slice_min() with prop = 0 selects no rows")
  expect_equal(nrow(slice_max(d, x, prop = 0)), 0L, info = "slice_max() with prop = 0 selects no rows")
  expect_equal(nrow(slice_sample(d, n = 0)), 0L, info = "slice_sample() with n = 0 selects no rows")
  expect_equal(nrow(slice_sample(d, prop = 0)), 0L, info = "slice_sample() with prop = 0 selects no rows")
}
expect_equal(names(slice_sample(df, n = 0)), c("g", "x", "id"), info = "slice_sample() with n = 0 keeps the columns")
expect_equal(group_vars(slice_sample(gf, prop = 0)), "g", info = "slice_sample() with prop = 0 keeps the groups")
for (n in c(-8, -10, -14)) {
  expect_equal(
    nrow(slice_min(df, x, n = n, with_ties = FALSE)), 0L,
    info = paste("slice_min() with with_ties = FALSE and n =", n, "larger than the data selects no rows")
  )
  expect_equal(
    nrow(slice_max(df, x, n = n, with_ties = FALSE)), 0L,
    info = paste("slice_max() with with_ties = FALSE and n =", n, "larger than the data selects no rows")
  )
}
expect_equal(
  slice_min(gf, x, n = -3, with_ties = FALSE)$id, 2L,
  info = "grouped slice_min() with with_ties = FALSE drops the groups no larger than a negative n"
)
expect_equal(
  slice_max(gf, x, n = -3, with_ties = FALSE)$id, 4L,
  info = "grouped slice_max() with with_ties = FALSE drops the groups no larger than a negative n"
)
expect_equal(
  nrow(slice_min(gf, x, prop = -1.5, with_ties = FALSE)), 0L,
  info = "grouped slice_min() with with_ties = FALSE and prop = -1.5 selects no rows"
)
expect_equal(
  nrow(slice_max(df, x, prop = -1.5, with_ties = FALSE)), 0L,
  info = "slice_max() with with_ties = FALSE and prop = -1.5 selects no rows"
)
rm(df, gf, d, n)
