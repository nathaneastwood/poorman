expect_equal(
  mtcars %>% count(cyl),
  data.frame(cyl = c(4, 6, 8), n = c(11L, 7L, 14L)),
  info = "count() works on a single grouping variable"
)

res <- mtcars %>% group_by(gear) %>% count(cyl)
expect_equal(
  attr(res, "groups"),
  structure(
    list(gear = c(3, 4, 5), .rows = list(1:3, 4:5, 6:8)),
    class = "data.frame", row.names = c(NA, -3L), .drop = TRUE
  ),
  info = "count() works on a grouped data.frame"
)
expect_equal(
  res %>% ungroup(),
  structure(
    list(gear = c(3, 3, 3, 4, 4, 5, 5, 5), cyl = c(4, 6, 8, 4, 6, 4, 6, 8), n = c(1L, 2L, 12L, 8L, 4L, 2L, 1L, 2L)),
    class = "data.frame", row.names = c(NA, -8L)
  ),
  info = "count() works on a grouped data.frame"
)

expect_equal(
  mtcars %>% count(cyl, wt = am),
  data.frame(cyl = c(4, 6, 8), n = c(8, 3, 2)),
  info = "count() can perform weighted counts"
)

expect_equal(
  mtcars %>% count(cyl, sort = TRUE, name = "freq"),
  data.frame(cyl = c(8, 4, 6), freq = c(14, 11, 7)),
  info = "count() is able to sort and rename the frequency column"
)

expect_equal(
  mtcars %>% tally(),
  data.frame(n = 32),
  info = "tally() works on non-grouped data.frames"
)

expect_equal(
  mtcars %>% group_by(cyl) %>% tally(),
  data.frame(cyl = c(4, 6, 8), n = c(11L, 7L, 14L)),
  info = "tally() works on grouped data.frames"
)

expect_equal(
  mtcars %>% tally(wt = am),
  data.frame(n = 13),
  info = "tally() can perform weighted counts"
)

expect_equal(
  mtcars %>% group_by(cyl) %>% tally(wt = am),
  data.frame(cyl = c(4, 6, 8), n = c(8, 3, 2)),
  info = "tally() can perform weighted counts on grouped data.frames"
)

expect_equal(
  mtcars %>% group_by(cyl) %>% tally(sort = TRUE, name = "freq"),
  data.frame(cyl = c(8, 4, 6), freq = c(14L, 11L, 7L)),
  info = "tally() can sort and rename the frequency variable"
)

exp <- mtcars %>% add_count(cyl, wt = am, sort = TRUE, name = "freq")
expect_equal(
  exp,
  {
    res <- mtcars
    res[, "freq"] <- c(3, 3, 8, 3, 2, 3, 2, 8, 8, 3, 3, 2, 2, 2, 2, 2, 2, 8, 8, 8, 8, 2, 2, 2, 2, 8, 8, 8, 2, 3, 2, 8)
    res[c(3, 8, 9, 18:21, 26:28, 32, 1, 2, 4, 6, 10, 11, 30, 5, 7, 12:17, 22:25, 29, 31), ]
  },
  info = "add_count() returns the expected data.frame, sorted and ungrouped"
)

expect_equal(
  mtcars %>% group_by(cyl) %>% add_tally(wt = am, sort = TRUE, name = "freq") %>% ungroup(),
  {
    res <- mtcars
    res <- res[order(mtcars$cyl), ]
    res[, "freq"] <- c(8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 3, 3, 3, 3, 3, 3, 3, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2)
    res
  },
  info = "tally() returns the expected data.frame"
)

expect_equal(
  mtcars %>% group_by(cyl) %>% add_tally() %>% ungroup(),
  {
    res <- mtcars
    res[, "n"] <- c(
      7L, 7L, 11L, 7L, 14L, 7L, 14L, 11L, 11L, 7L, 7L, 14L, 14L, 14L, 14L, 14L, 14L, 11L, 11L, 11L, 11L, 14L, 14L, 14L,
      14L, 11L, 11L, 11L, 14L, 7L, 14L, 11L
    )
    res
  }
)

expect_error(
  mtcars %>% count(cyl, name = c("name_one", "name_two"))
)

expect_error(
  {
    res <- mtcars
    colnames(res)[1] <- "n"
    res %>% tally()
  }
)


# Grouping of the results
df <- data.frame(g = c(2, 1, 2, NA), h = c("x", "y", "x", "y"), v = 1:4, stringsAsFactors = FALSE)
expect_silent(tally(group_by(df, h, g)), info = "tally() does not report the grouping of its summary")
expect_equal(
  group_vars(tally(group_by(df, h, g))),
  "h",
  info = "tally() drops the last grouping variable"
)
expect_silent(count(group_by(df, h), g), info = "count() does not report the grouping of its summary")
expect_equal(
  group_vars(count(group_by(df, h, g), v)),
  c("h", "g"),
  info = "count() keeps every grouping variable of its input"
)
expect_false(
  group_by_drop_default(count(group_by(df, h, .drop = FALSE), g)),
  info = "count() keeps the .drop setting of its input"
)

ge_df <- data.frame(x = 1:4, g = c("a", "a", "b", "b"))
expect_equal(
  count(ge_df, tot = g),
  data.frame(tot = c("a", "b"), n = c(2L, 2L)),
  info = "count() counts a renamed column"
)
ge_fn <- function(data) {
  k <- 2L
  count(data, h = x %/% k)
}
expect_equal(
  ge_fn(ge_df),
  data.frame(h = 0:2, n = c(1L, 2L, 1L)),
  info = "count() finds variables in the calling environment"
)

fe_df <- data.frame(g = c(1, 1, 2), x = 1:3)
fe_k <- 100L
fe_inner <- function(data) {
  off <- 10L
  mutate(data, y = x + off)$y[1L]
}
expect_equal(
  count(fe_df, m = fe_inner(fe_df)),
  data.frame(m = 11L, n = 3L),
  info = "a mutate() called inside count() finds variables in its own calling environment"
)
fe_fn <- function(data) {
  fe_k <- 2L
  nm <- "new"
  list(
    count(data, h = x %% fe_k),
    add_count(data, h = x %% fe_k),
    names(count(data, "{nm}" := x)), # nolint: object_name_linter.
    names(add_count(data, "{nm}" := x)) # nolint: object_name_linter.
  )
}
expect_equal(
  fe_fn(fe_df),
  list(
    data.frame(h = 0:1, n = 1:2),
    data.frame(g = c(1, 1, 2), x = 1:3, h = c(1L, 0L, 1L), n = c(2L, 1L, 2L)),
    c("new", "n"),
    c("g", "x", "new", "n")
  ),
  info = "count() and add_count() use a local variable over a global one, also in a := template"
)

ac_df <- data.frame(
  g = c("b", "a", "b", "a", "b"),
  h = factor(c("u", "v", "u", "u", "v"), levels = c("u", "v", "w")),
  w = 1:5,
  stringsAsFactors = FALSE
)
expect_equal(
  add_count(ac_df, g),
  data.frame(ac_df, n = c(3L, 2L, 3L, 2L, 3L)),
  info = "add_count() on ungrouped data returns ungrouped data"
)
res <- add_count(group_by(ac_df, h, .drop = FALSE), g)
expect_equal(
  list(group_vars(res), group_by_drop_default(res), res$n),
  list("h", FALSE, c(2L, 1L, 2L, 1L, 1L)),
  info = "add_count() returns the groups and .drop of its input"
)
res <- add_count(group_by(ac_df, g))
expect_equal(
  list(group_vars(res), res$n),
  list("g", c(3L, 2L, 3L, 2L, 3L)),
  info = "add_count() without ... keeps groups"
)
res <- add_count(ac_df, g, sort = TRUE)
expect_equal(
  list(res$w, res$n, group_vars(res)),
  list(c(1L, 3L, 5L, 2L, 4L), c(3L, 3L, 3L, 2L, 2L), character(0L)),
  info = "add_count(sort = TRUE) sorts the rows by the count, largest first"
)
res <- add_count(group_by(ac_df, h), g, sort = TRUE)
expect_equal(
  list(res$w, res$n, group_vars(res)),
  list(c(1L, 3L, 2L, 4L, 5L), c(2L, 2L, 1L, 1L, 1L), "h"),
  info = "add_count(sort = TRUE) sorts all rows, not within the groups of its input"
)
expect_equal(
  add_count(ac_df, g, wt = w)$n,
  c(9L, 6L, 9L, 6L, 9L),
  info = "add_count() with wt sums the weights in each group"
)

res <- add_tally(group_by(ac_df, h, .drop = FALSE))
expect_equal(
  list(group_vars(res), group_by_drop_default(res), res$n),
  list("h", FALSE, c(3L, 2L, 3L, 3L, 2L)),
  info = "add_tally() returns the groups and .drop of its input"
)

# `add_tally()` and `add_count()` add their column through the `mutate()` generic and keep the class of their input
ac_calls <- 0L
registerS3method("mutate", "ac_class", function(.data, ...) {
  ac_calls <<- ac_calls + 1L
  NextMethod()
}, envir = asNamespace("poorman"))
ac_cls <- structure(data.frame(g = c(1, 1, 2), x = 1:3), class = c("ac_class", "data.frame"))
ac_res <- lapply(
  list(
    function() add_tally(ac_cls),
    function() add_count(ac_cls),
    function() add_count(ac_cls, g),
    function() add_tally(group_by(ac_cls, g)),
    function() add_count(group_by(ac_cls, g), x)
  ),
  function(f) {
    ac_calls <<- 0L
    res <- f()
    list(ac_calls > 0L, inherits(res, "ac_class"), group_vars(res))
  }
)
expect_equal(
  ac_res,
  list(
    list(TRUE, TRUE, character(0L)),
    list(TRUE, TRUE, character(0L)),
    list(TRUE, TRUE, character(0L)),
    list(TRUE, TRUE, "g"),
    list(TRUE, TRUE, "g")
  ),
  info = "add_tally() and add_count() call mutate() methods, keep the class and return the groups of their input"
)

# `wt` is evaluated as an expression of the columns and the variables of the calling environment
wt_df <- data.frame(g = c("a", "b", "a", "b", "b"), x = c(1L, 2L, 3L, NA, 5L), y = c(0.5, 1, NA, 2, 1.5))
# nolint start: object_usage_linter. `x` and `y` are columns, not global variables.
wt_fn <- function(data) {
  k <- 10
  list(
    count(data, g, wt = x * k),
    tally(group_by(data, g), wt = x * k),
    add_count(data, g, wt = x * k)$n,
    add_tally(group_by(data, g), wt = x * k)$n,
    tally(data, wt = x * k),
    add_count(data, g, wt = y + k, sort = TRUE)$x
  )
}
wt_local <- function(data) {
  z <- 1
  w <- NULL
  list(add_count(data, wt = z)$n, count(data, g, wt = z), tally(data, wt = w))
}
wt_mask <- function(data) {
  x <- 100L
  count(data, g, wt = x)
}
# nolint end
expect_equal(
  wt_fn(wt_df),
  list(
    data.frame(g = c("a", "b"), n = c(40, 70)),
    data.frame(g = c("a", "b"), n = c(40, 70)),
    c(40, 70, 40, 70, 70),
    c(40, 70, 40, 70, 70),
    data.frame(n = 110),
    c(2L, NA, 5L, 1L, 3L)
  ),
  info = "count(), tally(), add_count() and add_tally() take an expression of columns and local variables as wt"
)
expect_equal(
  wt_local(wt_df),
  list(c(1, 1, 1, 1, 1), data.frame(g = c("a", "b"), n = c(1, 1)), data.frame(n = 0L)),
  info = "wt can be a local variable which is not a column, including NULL"
)
expect_equal(
  wt_mask(wt_df),
  data.frame(g = c("a", "b"), n = c(4L, 7L)),
  info = "a column used as wt masks a local variable of the same name"
)
expect_equal(
  list(count(wt_df, g, wt = x), count(wt_df, g, wt = y), count(wt_df, g, wt = 2)),
  list(
    data.frame(g = c("a", "b"), n = c(4L, 7L)),
    data.frame(g = c("a", "b"), n = c(0.5, 4.5)),
    data.frame(g = c("a", "b"), n = c(2, 2))
  ),
  info = "count() sums the non-missing weights of a column, keeping its type, or a constant weight"
)
expect_error(count(wt_df, g, wt = "x"), info = "count() does not take a column name as a string for wt")
expect_warning(
  tally(group_by(wt_df, g), wt = n()),
  "`wt = n()` is deprecated",
  fixed = TRUE,
  info = "tally() warns that wt = n() is deprecated"
)
expect_equal(
  suppressWarnings(tally(group_by(wt_df, g), wt = n())),
  data.frame(g = c("a", "b"), n = 2:3),
  info = "tally() with wt = n() counts the rows"
)

wt_n <- data.frame(g = c("a", "a", "b"), n = c(5L, 6L, 7L))
expect_silent(add_tally(wt_n, name = "nn"), info = "add_tally() does not report a column n as a weight")
expect_equal(
  list(
    count(wt_n, g, name = "nn"),
    tally(wt_n, name = "nn"),
    add_count(wt_n, g, name = "nn")$nn,
    add_tally(wt_n, name = "nn")$nn,
    count(wt_n, g, wt = n, name = "nn")
  ),
  list(
    data.frame(g = c("a", "b"), nn = 2:1),
    data.frame(nn = 3L),
    c(2L, 2L, 1L),
    c(3L, 3L, 3L),
    data.frame(g = c("a", "b"), nn = c(11L, 7L))
  ),
  info = "a column n is used as a weight only when it is given as wt"
)
