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
  do.call(rbind, unname(lapply(split(mtcars, list(mtcars$am, mtcars$cyl)), function(x) x[1:2, ]))),
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
  poorman:::check_slice_size(n = c(1, 2)),
  info = "`n` must be a single number."
)

expect_error(
  poorman:::check_slice_size(n = NA),
  info = "`n` must be a non-missing positive number."
)

expect_error(
  poorman:::check_slice_size(n = -1),
  info = "`n` must be a non-missing positive number."
)

expect_error(
  poorman:::check_slice_size(prop = c(1, 2)),
  info = "`prop` must be a single number."
)

expect_error(
  poorman:::check_slice_size(prop = NA),
  info = "`prop` must be a non-missing positive number."
)

expect_error(
  poorman:::check_slice_size(prop = -1),
  info = "`prop` must be a non-missing positive number."
)

expect_error(
  poorman:::check_slice_size(n = 1, prop = 0.1),
  info = "Must supply exactly one of `n` and `prop` arguments."
)

rm(df)
