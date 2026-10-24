x <- data.frame(a = c(1L, 2L, 3L, NA), b = c("p", "q", "r", "s"), stringsAsFactors = FALSE)
y <- data.frame(a = c(1, 1, 3, NA, 5), z = c("u", "v", "w", "x", "y"), b = 1:5, stringsAsFactors = FALSE)

nested <- function(...) data.frame(..., stringsAsFactors = FALSE)
y_nested <- list(
  nested(z = c("u", "v"), b = 1:2),
  nested(z = character(0), b = integer(0)),
  nested(z = "w", b = 3L),
  nested(z = "x", b = 4L)
)

res <- nest_join(x, y, by = "a")
expect_equal(
  res,
  structure(
    list(a = c(1, 2, 3, NA), b = c("p", "q", "r", "s"), y = y_nested),
    class = "data.frame",
    row.names = c(NA, -4L)
  ),
  info = "nest_join() nests the matching rows of y, without the join keys, in a column named after y"
)
expect_identical(
  lapply(res$y, rownames), list(c("1", "2"), character(0), "1", "1"), info = "Nested rows are renumbered"
)

expect_equal(
  nest_join(x, y, by = "a", keep = TRUE)$y,
  list(
    nested(a = c(1, 1), z = c("u", "v"), b = 1:2),
    nested(a = numeric(0), z = character(0), b = integer(0)),
    nested(a = 3, z = "w", b = 3L),
    nested(a = NA_real_, z = "x", b = 4L)
  ),
  info = "nest_join(keep = TRUE) keeps the join keys of y in the nested data.frames"
)

expect_equal(
  names(nest_join(x, y, by = "a", name = "matches")),
  c("a", "b", "matches"),
  info = "nest_join() uses `name` for the nested column"
)

res <- nest_join(x, y, by = "a", name = "b")
expect_equal(names(res), c("a", "b"), info = "A `name` matching a column of x replaces that column in place")
expect_equal(res$b, y_nested, info = "A `name` matching a column of x replaces that column in place")

expect_equal(
  names(x %>% nest_join(y %>% filter(a > 0), by = "a")),
  c("a", "b", "y %>% filter(a > 0)"),
  info = "The default `name` is the expression given for y"
)

expect_message(nest_join(x, y[, c("a", "z")]), "Joining, by = \"a\"", info = "A natural join reports its keys")
expect_equal(
  suppressMessages(nest_join(x, y[, c("a", "z")]))[["y[, c(\"a\", \"z\")]"]],
  list(nested(z = c("u", "v")), nested(z = character(0)), nested(z = "w"), nested(z = "x")),
  info = "A natural join joins by the common columns"
)

res <- nest_join(x, y, by = c("a" = "b"))
expect_identical(res$a, c(1L, 2L, 3L, NA), info = "Keys of the same type are not cast")
expect_equal(
  res$y,
  list(
    nested(a = 1, z = "u"), nested(a = 1, z = "v"), nested(a = 3, z = "w"), nested(a = numeric(0), z = character(0))
  ),
  info = "nest_join() joins different columns with a named `by`"
)

expect_equal(
  nest_join(x, y, by = character())$y,
  rep(list(y), 4L),
  info = "nest_join(by = character()) nests all of y in every row"
)

expect_equal(
  nest_join(data.frame(a = c(1, NaN, NA)), data.frame(a = c(NA, NaN), q = 1:2), by = "a")[[2L]],
  list(data.frame(q = integer(0)), data.frame(q = 2L), data.frame(q = 1L)),
  info = "NA matches NA and NaN matches NaN"
)

expect_equal(
  nest_join(x[0L, ], y, by = "a"),
  structure(
    list(a = numeric(0), b = character(0), y = list()),
    class = "data.frame",
    row.names = integer(0)
  ),
  info = "nest_join() works with zero row x"
)
expect_equal(
  nest_join(x, y[0L, ], by = "a")[[3L]],
  rep(list(nested(z = character(0), b = integer(0))), 4L),
  info = "nest_join() gives 0-row data.frames for zero row y"
)

res <- nest_join(group_by(x, a), y, by = "a")
expect_equal(group_vars(res), "a", info = "nest_join() keeps the groups of x")
expect_equal(group_data(res)$a, c(1, 2, 3, NA), info = "The groups of x are recalculated after casting the keys")

res <- nest_join(x, group_by(y, z), by = "a", name = "y")
expect_equal(class(res), "data.frame", info = "The groups of y are not kept")
expect_equal(res$y, y_nested, info = "The nested data.frames from a grouped y are not grouped")

f1 <- data.frame(k = factor(c("a", "b")), v = 1:2)
res <- nest_join(f1, data.frame(k = factor(c("b", "c")), w = 3:4), by = "k", name = "y")
expect_equal(res$k, factor(c("a", "b"), levels = c("a", "b", "c")), info = "Factor keys combine their levels")
expect_equal(res$y, list(data.frame(w = integer(0)), data.frame(w = 3L)), info = "Factor keys match by value")

res <- nest_join(f1, data.frame(k = c("b", "c"), w = 3:4, stringsAsFactors = FALSE), by = "k", name = "y")
expect_identical(res$k, c("a", "b"), info = "A factor key combines with a character key to a character key")
expect_equal(res$y, list(data.frame(w = integer(0)), data.frame(w = 3L)), info = "Factor and character keys match")

res <- nest_join(
  data.frame(d = as.Date("2020-01-01")),
  data.frame(d = as.POSIXct("2020-01-01", tz = "UTC"), v = 1),
  by = "d",
  name = "y"
)
expect_equal(res$d, as.POSIXct("2020-01-01", tz = "UTC"), info = "A Date key combines with a POSIXct key")
expect_equal(res$y, list(data.frame(v = 1)), info = "A Date key matches a POSIXct key")

res <- nest_join(data.frame(a = c(NA, NA)), data.frame(a = c("x", NA), v = 1:2), by = "a", name = "y")
expect_identical(res$a, c(NA_character_, NA_character_), info = "An all NA logical key takes the type of the other key")
expect_equal(res$y, list(data.frame(v = 2L), data.frame(v = 2L)), info = "An all NA logical key matches NA")

expect_error(
  nest_join(x, data.frame(a = c("1", "2")), by = "a"),
  "Can't combine `a` <integer> and `a` <character>.",
  fixed = TRUE,
  info = "nest_join() errors on keys of incompatible types"
)
expect_error(
  nest_join(x, y, by = "nope"),
  "Join columns must be present in data",
  info = "nest_join() errors on missing join columns"
)
expect_error(
  nest_join(x, data.frame(q = 1)),
  "`by` must be supplied when `x` and `y` have no common variables",
  info = "nest_join() errors when a natural join has no common columns"
)
