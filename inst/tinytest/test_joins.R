df1 <- data.frame(
  id = c(1, 3, 5, 7, 9),
  name = LETTERS[1:5],
  height = c(1, 2, 2, 2, 2),
  stringsAsFactors = FALSE
)

df2 <- data.frame(
  id = c(2, 4, 5, 7),
  name = LETTERS[1:4],
  weight = c(2, 3, 4, 5),
  stringsAsFactors = FALSE
)

expect_equal(
  {
    invisible(suppressMessages(res <- df1 %>% inner_join(df2)))
    res
  },
  data.frame(
    id = c(5, 7),
    name = LETTERS[3:4],
    height = c(2, 2),
    weight = c(4, 5),
    stringsAsFactors = FALSE
  ),
  info = "Inner join"
)

expect_equal(
  {
    invisible(suppressMessages(res <- df1 %>% left_join(df2)))
    res
  },
  data.frame(
    id = c(1, 3, 5, 7, 9),
    name = LETTERS[1:5],
    height = c(1, 2, 2, 2, 2),
    weight = c(NA, NA, 4, 5, NA),
    stringsAsFactors = FALSE
  ),
  info = "Left join"
)

expect_equal(
  {
    invisible(suppressMessages(res <- df1 %>% right_join(df2)))
    res
  },
  data.frame(
    id = c(5, 7, 2, 4),
    name = LETTERS[c(3, 4, 1, 2)],
    height = c(2, 2, NA, NA),
    weight = c(4, 5, 2, 3),
    stringsAsFactors = FALSE
  ),
  info = "Right join"
)

expect_equal(
  {
    invisible(suppressMessages(res <- df1 %>% full_join(df2)))
    res
  },
  data.frame(
    id = c(1, 3, 5, 7, 9, 2, 4),
    name = c("A", "B", "C", "D", "E", "A", "B"),
    height = c(1, rep(2, 4), NA, NA),
    weight = c(NA, NA, 4, 5, NA, 2, 3),
    stringsAsFactors = FALSE
  ),
  info = "Full join"
)

df1 <- data.frame(
  a = c(1, 2, 3, 4, 5, 6),
  b = c("a", "b", "f", "e", "r", "h"),
  d = c("q", "l", "o", "n", "q", "z"),
  stringsAsFactors = FALSE
)

df2 <- data.frame(
  a = c(1, 2, 3, 4, 5, 6),
  d = c("q", "l", "o", "n", "q", "z"),
  stringsAsFactors = FALSE
)

expect_equal(
  df1 %>% left_join(df2, by = c("a", "d")),
  data.frame(
    a = c(1, 2, 3, 4, 5, 6),
    b = c("a", "b", "f", "e", "r", "h"),
    d = c("q", "l", "o", "n", "q", "z"),
    stringsAsFactors = FALSE
  ),
  info = "Explicit `by` parameter"
)

expect_equal(
  df1 %>% left_join(df2, by = "a", suffix = c("_x", "_y")) %>% colnames(),
  c("a", "b", "d_x", "d_y"),
  info = "Suffix changes"
)

df1 <- data.frame(
  a1 = c(1, 2, 3, 4, 5, 6),
  b = c("a", "b", "f", "e", "r", "h"),
  d = c("q", "l", "o", "n", "q", "z"),
  stringsAsFactors = FALSE
)

df2 <- data.frame(
  a2 = c(1, 2, 3, 4, 5, 6),
  d = c("q", "l", "o", "n", "q", "z"),
  stringsAsFactors = FALSE
)

expect_equal(
  df1 %>% left_join(df2, by = c("a1" = "a2", "d" = "d")),
  data.frame(
    a1 = c(1, 2, 3, 4, 5, 6),
    b = c("a", "b", "f", "e", "r", "h"),
    d = c("q", "l", "o", "n", "q", "z"),
    stringsAsFactors = FALSE
  ),
  info = "Non-matching column names"
)

df1 <- data.frame(x = c(1, NA), y = 2)
df2 <- data.frame(x = c(1, NA), z = 3)
expect_equal(
  invisible(suppressMessages(res <- full_join(df1, df2, suffix = c(".xx", ".yy"), keep = TRUE, na_matches = "never"))),
  data.frame(
    x.xx = c(1, NA, NA),
    y = c(2, 2, NA),
    x.yy = c(1, NA, NA),
    z = c(3, NA, 3)
  ),
  info = "keep = TRUE keeps the join columns"
)

# Join keys are cast to their common type
x <- data.frame(k = 1:2, a = 1:2)
y <- data.frame(k = c(2, 3), b = 1:2)
expect_identical(inner_join(x, y, by = "k")$k, 2, info = "inner_join() casts integer and double keys to double")
expect_identical(left_join(x, y, by = "k")$k, c(1, 2), info = "left_join() casts integer and double keys to double")
expect_identical(right_join(x, y, by = "k")$k, c(2, 3), info = "right_join() casts integer and double keys to double")
expect_identical(full_join(x, y, by = "k")$k, c(1, 2, 3), info = "full_join() casts integer and double keys to double")

x <- data.frame(k = factor(c("p", "q")), a = 1:2)
y <- data.frame(k = c("q", "r"), b = 1:2, stringsAsFactors = FALSE)
expect_identical(inner_join(x, y, by = "k")$k, "q", info = "inner_join() casts factor and character keys to character")
expect_identical(
  left_join(x, y, by = "k")$k, c("p", "q"), info = "left_join() casts factor and character keys to character"
)
expect_identical(
  right_join(x, y, by = "k")$k, c("q", "r"), info = "right_join() casts factor and character keys to character"
)
expect_identical(
  full_join(x, y, by = "k")$k, c("p", "q", "r"), info = "full_join() casts factor and character keys to character"
)

y <- data.frame(k = factor(c("q", "r")), b = 1:2)
expect_identical(
  inner_join(x, y, by = "k")$k, factor("q", levels = c("p", "q", "r")), info = "inner_join() combines factor levels"
)
expect_identical(
  left_join(x, y, by = "k")$k,
  factor(c("p", "q"), levels = c("p", "q", "r")),
  info = "left_join() combines factor levels"
)

x <- data.frame(k = c(NA, NA), a = 1:2)
y <- data.frame(k = c("q", NA), b = 1:2, stringsAsFactors = FALSE)
expect_identical(
  left_join(x, y, by = "k")$k,
  c(NA_character_, NA_character_),
  info = "An all NA logical key takes the type of the other key"
)

x <- data.frame(k = as.Date(c("2020-01-01", "2020-01-02")), a = 1:2)
y <- data.frame(k = as.POSIXct(c("2020-01-02", "2020-01-03"), tz = "UTC"), b = 1:2)
expect_identical(
  left_join(x, y, by = "k"),
  data.frame(k = as.POSIXct(c("2020-01-01", "2020-01-02"), tz = "UTC"), a = 1:2, b = c(NA, 1L)),
  info = "left_join() casts Date keys to POSIXct"
)

x <- data.frame(k = 1:2, a = 1:2)
expect_error(
  left_join(x, data.frame(j = c("1", "2"), b = 1:2), by = c(k = "j")),
  paste0(
    "Can't join on `x$k` x `y$k` because of incompatible types.\n",
    "i `x$k` is of type <integer>.\n",
    "i `y$k` is of type <character>."
  ),
  fixed = TRUE,
  info = "Mutating joins error on keys of incompatible types"
)
expect_error(
  inner_join(x, data.frame(b = 1:2), by = "nope"),
  "Join columns must be present in data",
  info = "Mutating joins error on missing join columns"
)
expect_error(
  full_join(x, data.frame(b = 1:2)),
  "`by` must be supplied when `x` and `y` have no common variables",
  info = "Natural mutating joins error when there are no common columns"
)
rm(x, y)

x <- data.frame(k = c(3L, 1L, 2L), v = c("c", "a", "b"), w = 1:3, stringsAsFactors = FALSE)
y <- data.frame(w = c(9L, 8L, 7L, 6L), m = c(2L, 9L, 3L, 5L), k = c(20L, 90L, 30L, 50L))
expect_identical(
  full_join(x, y, by = c(k = "m"), keep = TRUE),
  data.frame(
    k.x = c(3L, 1L, 2L, NA, NA), v = c("c", "a", "b", NA, NA), w.x = c(1L, 2L, 3L, NA, NA),
    w.y = c(7L, NA, 9L, 8L, 6L), m = c(3L, NA, 2L, 9L, 5L), k.y = c(30L, NA, 20L, 90L, 50L),
    stringsAsFactors = FALSE
  ),
  info = "full_join(keep = TRUE) keeps both keys, with NA where a row has no match"
)
expect_identical(
  left_join(x, y, by = c(k = "m"), keep = TRUE),
  data.frame(
    k.x = c(3L, 1L, 2L), v = c("c", "a", "b"), w.x = 1:3,
    w.y = c(7L, NA, 9L), m = c(3L, NA, 2L), k.y = c(30L, NA, 20L),
    stringsAsFactors = FALSE
  ),
  info = "left_join(keep = TRUE) works with a named `by`"
)
expect_identical(
  names(inner_join(x, y, by = c(k = "m"), keep = TRUE)),
  c("k.x", "v", "w.x", "w.y", "m", "k.y"),
  info = "inner_join() supports keep = TRUE"
)
expect_identical(
  right_join(x, y, by = c(k = "m")),
  data.frame(
    k = c(3L, 2L, 9L, 5L), v = c("c", "b", NA, NA), w.x = c(1L, 3L, NA, NA), w.y = c(7L, 9L, 8L, 6L),
    k.y = c(30L, 20L, 90L, 50L), stringsAsFactors = FALSE
  ),
  info = "Rows of y without a match come last, in their order in y, and take their keys from y"
)
expect_identical(
  names(left_join(data.frame(k = 1:2, k.x = 3:4), data.frame(k = 1:2, k.y = 5:6), by = "k", keep = TRUE)),
  c("k.x", "k.x.x", "k.y", "k.y.y"),
  info = "Suffixes are repeated until the names are unique"
)
expect_identical(
  names(full_join(x, y[c("m", "k")], by = character())),
  c("k.x", "v", "w", "m", "k.y"),
  info = "A cross join suffixes the columns found in both tables"
)
expect_identical(
  full_join(data.frame(k = c(NA, NaN, 1)), data.frame(k = c(NaN, NA, NA), z = 1:3), by = "k", na_matches = "never"),
  data.frame(k = c(NA, NaN, 1, NaN, NA, NA), z = c(NA, NA, NA, 1L, 2L, 3L)),
  info = "With na_matches = 'never', rows of y without a match keep their order"
)
expect_identical(
  left_join(x, y, by = c(k = "m"), sort = TRUE, foo = 1),
  left_join(x, y, by = c(k = "m")),
  info = "Arguments in `...` are ignored"
)
for (join in list(inner_join, left_join, right_join, full_join)) {
  expect_identical(
    names(formals(join)),
    c("x", "y", "by", "copy", "suffix", "...", "keep", "na_matches"),
    info = "The mutating joins take their arguments in the documented order"
  )
}
expect_identical(
  names(left_join(data.frame(k = 1:2, v = 1:2), data.frame(k = 1:2, v = 3:4), "k", FALSE, c("_l", "_r"))),
  c("k", "v_l", "v_r"),
  info = "`suffix` is the fifth argument, after `copy`"
)
expect_identical(
  left_join(x, y, by = c(k = "m"), copy = TRUE),
  left_join(x, y, by = c(k = "m")),
  info = "`copy` is ignored"
)
res <- left_join(group_by(x, k), y, by = c(k = "m"), keep = TRUE)
expect_identical(class(res), "data.frame", info = "Data which loses its grouping column is no longer grouped")
res <- left_join(group_by(x, k, v), y, by = c(k = "m"))
expect_identical(group_vars(res), c("k", "v"), info = "Joins keep the groups of x")
rm(x, y, res)
