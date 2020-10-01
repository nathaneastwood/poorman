one <- mtcars[1:4, ]
two <- mtcars[9:12, ]

expect_equal(
  bind_rows(one, two),
  rbind(one, two),
  info = "data.frames can be supplied as arguments"
)

expect_equal(
  bind_rows(list(one, two)),
  rbind(one, two),
  info = "data.frames can be supplied in a list"
)
expect_equal(
  bind_rows(split(mtcars, mtcars$cyl)),
  mtcars[order(mtcars$cyl), ],
  info = "contents of lists are spliced automatically"
)
expect_equal(
  bind_rows(list(one, two), list(two, one)),
  rbind(one, two, two, one),
  info = "contents of lists are spliced automatically"
)

expect_equal(
  bind_rows(c(a = 1, b = 2), c(a = 3, b = 4)),
  data.frame(a = c(1, 3), b = c(2, 4)),
  info = "Vectors can be supplied"
)

expect_error(
  bind_rows(c(1, 2), c(a = 3, b = 4)),
  info = "Vectors must be named"
)

expect_equal(
  bind_rows(c(a = 1, b = 2), data.frame(a = 3:4, b = 5:6), c(a = 7, b = 8)),
  data.frame(a = c(1, 3, 4, 7), b = c(2, 5, 6, 8)),
  info = "A mixture of vectors and data.frames can be supplied"
)

expect_equal(
  bind_rows(list(one, two), .id = "id"),
  {
    res <- rbind(one, two)
    res[, "id"] <- c(rep("1", 4), rep("2", 4))
    res[, c("id", "mpg", "cyl", "disp", "hp", "drat", "wt", "qsec", "vs", "am", "gear", "carb")]
  },
  info = "If no names are found a numeric sequence is used."
)
expect_equal(
  bind_rows(list(a = one, b = two), .id = "id"),
  {
    res <- rbind(one, two)
    res[, "id"] <- c(rep("a", 4), rep("b", 4))
    res[, c("id", "mpg", "cyl", "disp", "hp", "drat", "wt", "qsec", "vs", "am", "gear", "carb")]
  },
  info = "When a list of data.frames is supplied, the labels are taken from the names of the list."
)
expect_equal(
  bind_rows("group 1" = one, "group 2" = two, .id = "groups"),
  {
    res <- rbind(one, two)
    res[, "groups"] <- c(rep("group 1", 4), rep("group 2", 4))
    res[, c("groups", "mpg", "cyl", "disp", "hp", "drat", "wt", "qsec", "vs", "am", "gear", "carb")]
  },
  info = "Named dots are used for groups when supplied."
)

df <- data.frame(x = 1)
expect_equal(bind_rows(df, NULL), df, info = "bind_rows() handles NULL")
expect_equal(bind_rows(list(df, NULL)), df, info = "bind_rows() handles NULL")
rm(df)

expect_equal(
  bind_rows(c(z = 2), data.frame(x = 1:3), data.frame(y = 1:4)),
  data.frame(
    z = c(2, rep(NA_real_, 7)),
    x = c(NA_integer_, 1L:3L, rep(NA_integer_, 4)),
    y = c(rep(NA_integer_, 4), 1L:4L)
  ),
  info = "Columns missing from subsequent data.frames are filled with NAs"
)

expect_error(
  bind_rows(c(z = 2), data.frame(x = 1:3), y ~ x),
  info = "You must pass vector(s) and/or data.frame(s)."
)

# -- bind_cols() ---------------------------------------------------------------

expect_equal(
  bind_cols(one, two),
  cbind(one, two),
  info = "data.frames can be supplied as arguments"
)

expect_equal(
  bind_cols(c(1, 2), c(3, 4)),
  data.frame(V1 = c(1, 2), V2 = c(3, 4)),
  info = "vectors can be supplied as arguments"
)

expect_equal(
  bind_cols(one, test = c(1, 2, 3, 4)),
  {
    res <- mtcars[1:4, ]
    res[, "test"] <- 1:4
    res
  },
  info = c("A mixture of data.frames and lists can be passed")
)

expect_equal(
  bind_cols(list(one, two)),
  cbind(one, two),
  info = "A list of data.frames can be supplied as arguments"
)

expect_identical(
  bind_cols(NULL),
  data.frame(),
  info = "bind_cols() handles NULL"
)
expect_identical(
  bind_cols(list(a = NULL, b = NULL)),
  data.frame(),
  info = "bind_cols handles all-NULL values"
)

expect_equal(
  bind_cols(list(y = data.frame(x = 1:2), z = data.frame(y = 1:2))),
  data.frame(x = 1:2, y = 1:2),
  info = "bind_cols unpacks data.frames"
)

expect_error(
  bind_cols(data.frame(x = 1:3), data.frame(y = 1:2)),
  info = "Rows need to match when column-binding"
)

expect_error(
  bind_cols(data.frame(x = 1:3), data.frame()),
  info = "Rows need to match when column-binding even with 0 columns"
)

expect_error(
  bind_cols(list(y = data.frame(x = 1:2), y ~ x)),
  info = "You must pass vector(s) and/or data.frame(s)"
)

rm(one, two)
