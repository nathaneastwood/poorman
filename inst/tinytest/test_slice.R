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

expect_equal(
  data.frame(x = 1:5) %>% slice_head(n = 6) %>% nrow(),
  5,
  info = "slice_head() silently truncates results"
)

expect_equal(
  data.frame(x = 1:5) %>% slice_tail(n = 6) %>% nrow(),
  5,
  info = "slice_tail() silently truncates results"
)

expect_equal(
  data.frame(x = 1:10) %>% slice_head(prop = 0.11) %>% nrow(),
  1,
  info = "slice_head() returns the correct proportion"
)

expect_equal(
  data.frame(x = 1:10) %>% slice_tail(prop = 0.11) %>% nrow(),
  1,
  info = "slice_tail() returns the correct proportion"
)

expect_equal(
  mtcars %>% slice_head(),
  mtcars[1, ],
  info = "slice_head() defaults to n = 1"
)

expect_error(
  data.frame(1:10) %>% slice_head(n = c(1, 2)),
  info = "`n` must be a single number."
)

expect_error(
  mtcars %>% slice_head(n = NA),
  info = "`n` must be a non-missing positive number."
)

expect_error(
  mtcars %>% slice_head(n = -1),
  info = "`n` must be a non-missing positive number."
)

expect_error(
  mtcars %>% slice_head(prop = c(1, 2)),
  info = "`prop` must be a single number."
)

expect_error(
  mtcars %>% slice_head(prop = NA),
  info = "`prop` must be a non-missing positive number."
)

expect_error(
  mtcars %>% slice_head(prop = -1),
  info = "`prop` must be a non-missing positive number."
)

expect_error(
  mtcars %>% slice_head(n = 1, prop = 0.1),
  info = "Must supply exactly one of `n` and `prop` arguments."
)
