df <- data.frame(x = 1)
gf <- group_by(df, x)
expect_equal(mutate(df), df, info = "empty mutate() returns input")
expect_equal(mutate(gf), gf, info = "empty mutate() returns input for grouped data")

expect_equal(
  mtcars %>% mutate(mpg2 = mpg * 2),
  {
    res <- mtcars
    res[, "mpg2"] <- res$mpg * 2
    res
  },
  info = "Test mutate() can add a new column"
)

df <- data.frame(x = 1:3, y = 1:3)
gf <- group_by(df, x)
expect_equal(df %>% mutate(y = NULL), df[1], info = "mutate() can remove variables with NULL")
expect_equal(
  df %>% mutate(z = NULL),
  df,
  info = "mutate() can remove variables with NULL even if it doesn't exist"
)
expect_equal(
  df %>% mutate(z = 1, z = NULL),
  df,
  info = "mutate() can remove variables with NULL even if it was just created"
)

expect_equal(
  mtcars %>% mutate(mpg = mpg * 2),
  {
    res <- mtcars
    res[, "mpg"] <- res$mpg * 2
    res
  },
  info = "Test mutate() can transform existing columns"
)

df <- data.frame(x = 1)
expect_equal(
  df %>% mutate(y = x + 1, z = y + 1),
  data.frame(x = 1, y = 2, z = 3),
  info = "mutations applied progressively: 1"
)
expect_equal(
  df %>% mutate(x = x + 1, x = x + 1),
  data.frame(x = 3),
  info = "mutations applied progressively: 2"
)
expect_equal(
  df %>% mutate(x = 2, y = x),
  data.frame(x = 2, y = 2),
  info = "mutations applied progressively: 3"
)
df <- data.frame(x = 1, y = 2)
expect_equal(
  df %>% mutate(x2 = x, x3 = x2 + 1),
  df %>% mutate(x2 = x + 0, x3 = x2 + 1),
  info = "mutations applied progressively: 4"
)

expect_equal(
  mtcars %>% mutate(1L),
  {
    res <- mtcars
    res[, "1L"] <- 1L
    res
  },
  info = "Test that unnamed conditions are evaluated"
)

# Grouped Operations
expect_equal(
  mtcars %>% group_by(am, cyl) %>% mutate(mpg2 = mpg * 2) %>% ungroup(),
  {
    res <- mtcars
    res <- do.call(rbind, unname(lapply(
      split(res, list(res$am, res$cyl)),
      function(x) {
        x[, "mpg2"] <- x$mpg * 2
        x
      }
    )))
    res[rownames(mtcars), ]
  },
  info = "Test grouped mutations"
)

expect_equal(
  mtcars %>% group_by(am, cyl, gear) %>% mutate(col = 1) %>% ungroup(),
  {
    res <- mtcars
    res <- do.call(rbind, unname(lapply(
      split(res, list(res$am, res$cyl)),
      function(x) {
        x[, "col"] <- 1
        x
      }
    )))
    res[rownames(mtcars), ]
  },
  info = "mutate() works when there are missing groups"
)

df <- data.frame(x = 1, y = 2)
out <- df %>% mutate(z = {x <- 10; x})
expect_equal(out, data.frame(x = 1, y = 2, z = 10), info = "assignments don't overwrite variables")

df <- structure(list(), class = "data.frame", row.names = c(NA, -2L), .Names = character(0))
colnames(df) <- NULL
expect_equal(
  mutate(df, x = 1),
  data.frame(x = c(1, 1)),
  info = "can mutate a data frame with zero columns and `NULL` column names"
)

df <- data.frame(x = c(1, NA, NaN))
out <- mutate(df, y = x * 1)
expect_equal(out$y, df$x, info = "mutate() disambiguates NA and NaN")

expect_identical(
  data.frame(a = 1) %>% mutate(data.frame(b = 2)),
  data.frame(a = 1, b = 2),
  info = "unnamed data frames are automatically unspliced: 1"
)
expect_identical(
  data.frame(a = 1) %>% mutate(data.frame(b = 2), data.frame(b = 3)),
  data.frame(a = 1, b = 3),
  info = "unnamed data frames are automatically unspliced: 2"
)
expect_identical(
  data.frame(a = 1) %>% mutate(data.frame(b = 2), c = b),
  data.frame(a = 1, b = 2, c = 2),
  info = "unnamed data frames are automatically unspliced: 3"
)

df <- data.frame(x = 1)
out <- df %>% mutate(y = data.frame(a = x))
expect_equal(
  out,
  {
    res <- data.frame(x = 1)
    res[["y"]] <- data.frame(a = 1)
    res
  },
  info = "named data frames are packed"
)

gf <- group_by(data.frame(x = 1:2, y = 2), x)
out <- mutate(gf, x = 1)
expect_equal(group_vars(out), "x", info = "mutate() preserves grouping: 1")
expect_equal(nrow(group_data(out)), 1, info = "mutate() preserves grouping: 2")
out <- mutate(gf, z = 1)
expect_equal(group_data(out), group_data(gf), info = "mutate() preserves grouping: 3")

df <- data.frame()
res <- df %>% mutate()
expect_equal(nrow(res), 0L, info = "mutate() works on empty data frames: 1")
expect_equal(length(res), 0L, info = "mutate() works on empty data frames: 2")
res <- df %>% mutate(x = numeric())
expect_equal(names(res), "x", info = "mutate() works on empty data frames: 3")
expect_equal(nrow(res), 0L, info = "mutate() works on empty data frames: 4")
expect_equal(length(res), 1L, info = "mutate() works on empty data frames: 5")

d <- data.frame(subject = c("Jack", "Jill"), id = c(2, 1)) %>% group_by(subject)
a1 <- names(attributes(d))
a2 <- names(attributes(d %>% mutate(foo = 1)))
expect_equal(setdiff(a1, a2), character(0), info = "grouped mutate does not drop grouping attributes")

df <- data.frame(x = c(1, 2), y = c(1, NA))
res <- df %>% group_by(x) %>% mutate(z = ifelse(y > 1, 1, 2))
expect_true(is.na(res$z[2]), info = "mutate() coerces results from one group with all NA values: 1")
expect_true(inherits(res$z, "numeric"), info = "mutate() coerces results from one group with all NA values: 2")

# Evaluation envs
pass_through <- function(x) x
test_function <- function(mtcars_df) {
  pass_through_internal <- function(x) x
  mutate(.data = mtcars_df, carb = pass_through(carb), gear = pass_through_internal(gear))
}
expect_equal(
  test_function(mtcars),
  mtcars,
  info = "Ensure mutations occur in the correct enclosing environment"
)

test_function_grouped <- function(mtcars_df) {
  pass_through_internal_group <- function(x) x

  mtcars_df %>%
    group_by(cyl) %>%
    mutate(carb = pass_through(carb), gear = pass_through_internal_group(gear)) %>%
    ungroup()
}
expect_equal(
  test_function_grouped(mtcars),
  mtcars,
  info = "Ensure mutations occur in the correct enclosing environment for grouped data (#68)"
)

# List columns

df <- structure(list(), class = "data.frame", row.names = c(NA, -3L), .Names = character(0))
df[["x"]] <- list(1, 2:3, 4:6)
df[["y"]] <- 1:3
expect_equal(
  df %>% mutate(l = length(x)) %>% .[["l"]],
  c(3, 3, 3),
  info = "List columns can be mutated: 1"
)
expect_equal(
  df %>% mutate(l = lengths(x)) %>% .[["l"]],
  c(1, 2, 3),
  info = "List columns can be mutated: 2"
)
models <- mtcars %>% nest_by(cyl) %>% mutate(model = list(lm(mpg ~ wt, data = data)))
expect_equal(
  lapply(models$model, class),
  list("lm", "lm", "lm"),
  info = "List columns can be mutated: 3"
)

# .keep

df <- data.frame(x = 1, y = 2)
out <- mutate(df, x1 = x + 1, y = y, .keep = "unused")
expect_equal(colnames(out), c("y", "x1"), info = "`.keep` = 'unused' keeps variables explicitly mentioned")

df <- data.frame(x = 1, y = 2, z = 3, a = "a", b = "b", c = "c", stringsAsFactors = FALSE)
out <- mutate(df, across(where(is.numeric), identity), .keep = "unused")
expect_equal(colnames(out), colnames(df), info = ".keep = 'used' not affected by across()")

df <- data.frame(a = 1, b = 2, c = 3, x = 1, y = 2)
out <- mutate(df, xy = x + y, .keep = "used")
expect_equal(colnames(out), c("x", "y", "xy"), info = ".keep = 'used' keeps variables used in expressions")

test_df <- data.frame(x = 1:3, y = 1:3, z = 1:3)
var <- 1
test_expr <- quote(x * var + ifelse(x < y, 1, 0) + n())
used <- poorman:::find_used(test_expr)
expect_equal(
  used[used %in% colnames(test_df)],
  c("x", "y"),
  info = "find_used() works for variables used inside of functions"
)
rm(test_df, var, test_expr, used)

df <- data.frame(x = 1, y = 2)
gf <- group_by(df, x)
expect_equal(
  colnames(mutate(df, z = 1, .keep = "none")),
  "z",
  info = ".keep = 'none' only keeps mutated variables"
)
expect_equal(
  colnames(mutate(gf, z = 1, .keep = "none")),
  c("x", "z"),
  info = ".keep = 'none' only keeps grouping and mutated variables"
)

df <- data.frame(x = 1, y = 2)
expect_equal(
  colnames(mutate(df, y = 1, x = 2, .keep = "none")),
  c("y", "x"),
  info = ".keep = 'none' prefers new order"
)

gf <- group_by(df, x)
expect_equal(
  colnames(mutate(gf, y = 1, x = 2, .keep = "none")),
  c("y", "x"),
  info = ".keep = 'none' prefers new order even when grouped"
)

df <- data.frame(x = 1, y = 2, z = 3) %>% group_by(z)
expect_equal(
  df %>% mutate(a = x + 1, .keep = "none"),
  data.frame(z = 3, a = 2) %>% group_by(z),
  info = ".keep = 'none' always retains grouping variables"
)
expect_equal(
  df %>% mutate(a = x + 1, .keep = "all"),
  data.frame(x = 1, y = 2, z = 3, a = 2) %>% group_by(z),
  info = ".keep = 'all' always retains grouping variables"
)
expect_equal(
  df %>% mutate(a = x + 1, .keep = "used"),
  data.frame(x = 1, z = 3, a = 2) %>% group_by(z),
  info = ".keep = 'used' always retains grouping variables"
)
expect_equal(
  df %>% mutate(a = x + 1, .keep = "unused"),
  data.frame(y = 2, z = 3, a = 2) %>% group_by(z),
  info = ".keep = 'unused' always retains grouping variables"
)

# .before, .after

df <- data.frame(x = 1, y = 2)
expect_equal(
  colnames(mutate(df, z = 1)),
  c("x", "y", "z"),
  info = "lack of .before and .after doesn't affect column position"
)
expect_equal(
  colnames(mutate(df, z = 1, .before = 1)),
  c("z", "x", "y"),
  info = "can use .before to control column position"
)
expect_equal(
  colnames(mutate(df, z = 1, .after = 1)),
  c("x", "z", "y"),
  info = "can use .after to control column position"
)
expect_equal(
  colnames(mutate(df, x = 1, .after = y)),
  c("x", "y"),
  info = ".after/.before doesn't affect order of existing columns"
)

# Errors

df <- data.frame(x = 1:2, y = 1:2)
expect_error(df %>% mutate(y = NULL, a = sum(y)), info = "setting column to NULL makes it unavailable")
expect_error(
  df %>% group_by(x) %>% mutate(y = NULL, a = sum(y)),
  info = "setting column to NULL makes it unavailable for grouped df"
)

# Grouped mutate --------------------------------------------------------------

gf <- data.frame(a = c(2, 1, 2), b = 1:3) %>% group_by(a)
expect_equal(
  group_rows(mutate(gf, z = 1)),
  list(2L, c(1L, 3L)),
  info = "mutate() keeps the group rows in step with the data"
)
expect_equal(
  group_rows(transmute(gf, z = 1)),
  list(2L, c(1L, 3L)),
  info = "transmute() keeps the group rows in step with the data"
)
expect_equal(mutate(gf, f = b * 2L)$f, c(2L, 4L, 6L), info = "mutate() on grouped data accepts a column called `f`")
expect_equal(
  as.data.frame(ungroup(mutate(gf, .fn = 1, .drop = 2, .reorder = 3))),
  data.frame(a = c(2, 1, 2), b = 1:3, .fn = 1, .drop = 2, .reorder = 3),
  info = "mutate() on grouped data accepts columns named like internal arguments"
)

df <- data.frame(g = c("a", NA, "a", NA), v = 1:4, stringsAsFactors = FALSE)
expect_equal(
  ungroup(mutate(group_by(df, g), s = sum(v))),
  data.frame(g = c("a", NA, "a", NA), v = 1:4, s = c(4L, 6L, 4L, 6L), stringsAsFactors = FALSE),
  info = "mutate() keeps rows with missing grouping values"
)

df <- data.frame(g1 = numeric(0), g2 = integer(0), v = integer(0))
res <- ungroup(mutate(group_by(df, g1, g2), m = mean(v), id = cur_group_id()))
expect_equal(nrow(res), 0L, info = "mutate() on empty grouped data returns an empty data.frame #1")
expect_equal(colnames(res), c("g1", "g2", "v", "m", "id"), info = "mutate() on empty grouped data returns an empty data.frame #2")

rm(df, gf, res)

pron <- data.frame(x = 1:4, y = c(2, 4, 6, 8), g = c("a", "a", "b", "b"))
col <- "y"

expect_equal(
  mutate(pron, z = .data$y * 2, w = .data[[col]] + x),
  data.frame(pron, z = c(4, 8, 12, 16), w = c(3, 6, 9, 12)),
  info = "mutate() supports the .data pronoun"
)

expect_equal(
  mutate(pron, x = .data$x * 10L, z = .data$x, w = .data$z + 1L),
  data.frame(x = c(10L, 20L, 30L, 40L), y = pron$y, g = pron$g, z = c(10L, 20L, 30L, 40L), w = c(11L, 21L, 31L, 41L)),
  info = "The .data pronoun sees the columns made by earlier expressions in mutate()"
)

expect_equal(
  mutate(group_by(pron, g), s = sum(.data[[col]]))$s,
  c(6, 6, 14, 14),
  info = "The .data pronoun holds the rows of the current group in mutate()"
)

expect_equal(
  transmute(pron, z = .data[[col]] + x),
  data.frame(z = c(3, 6, 9, 12)),
  info = "transmute() supports the .data pronoun"
)

nm_df <- data.frame(x = 1:2, y = c(10, 20))
expect_equal(
  names(mutate(nm_df, x + y + x + y + x + y + x + y + x + y + x + y + x + y + x + y + x + y + x)),
  c("x", "y", "... + x"),
  info = "mutate() abbreviates long automatic names"
)
expect_equal(
  names(mutate(nm_df, {
    x * 2
  })),
  c("x", "y", "{ ... }"),
  info = "mutate() names a multi-line expression on one line"
)
expect_equal(
  mutate(nm_df, .data$y * 2, .data$x),
  data.frame(x = 1:2, y = c(10, 20), `.data$y * 2` = c(20, 40), check.names = FALSE),
  info = "mutate() names .data$x after the column, which it overwrites"
)
expect_equal(
  names(transmute(nm_df, x + y + x + y + x + y + x + y + x + y + x + y + x + y + x + y + x + y + x)),
  "... + x",
  info = "transmute() abbreviates long automatic names"
)

ce_df <- data.frame(x = 1:3, y = c(2, 4, 6))
ce_nm <- "tot"
expect_equal(
  mutate(ce_df, z := y * 2, "{ce_nm}" := z + x), # nolint: object_name_linter.
  data.frame(x = 1:3, y = c(2, 4, 6), z = c(4, 8, 12), tot = c(5, 10, 15)),
  info = "mutate() names := arguments after a symbol or a gluestick template"
)
ce_fn <- function(data) {
  local_nm <- "inner"
  mutate(group_by(data, x), "{local_nm}" := y * 2) # nolint: object_name_linter.
}
expect_equal(
  names(ce_fn(ce_df)),
  c("x", "y", "inner"),
  info = "Gluestick templates are filled in from the calling environment on grouped data"
)

ds_df <- data.frame(x = 1:3, y = c(2, 4, 6))
ds_var <- "y"
expect_equal(
  names(mutate(ds_df, .data[[ds_var]] * 2)),
  c("x", "y", ".data[[\"y\"]] * 2"),
  info = "mutate() names .data[[var]] after the value of var"
)
expect_equal(
  names(transmute(ds_df, .data[[ds_var]], -.data[[ds_var]])),
  c("y", "-.data[[\"y\"]]"),
  info = "transmute() names .data[[var]] after the value of var"
)

ch_df <- data.frame(g = c("b", "a", "b", "c", "a"), x = 1:5)
ch_res <- ch_df %>% group_by(g) %>% mutate(k = 1) %>% mutate(z = n())
expect_equal(
  ungroup(ch_res),
  data.frame(g = c("b", "a", "b", "c", "a"), x = 1:5, k = 1, z = c(2L, 2L, 2L, 1L, 2L)),
  info = "The result of a grouped mutate() can be passed to another grouped verb"
)
expect_equal(group_rows(ch_res), list(c(2L, 5L), c(1L, 3L), 4L), info = "Chained grouped mutate() keeps the groups")

eg_df <- data.frame(g = factor(c("a", "a"), levels = c("a", "b")), x = 1:2)
eg_seen <- character()
eg_res <- mutate(group_by(eg_df, g, .drop = FALSE), k = {
  eg_seen <<- c(eg_seen, paste0(cur_group_id(), ":", n()))
  1
})
expect_equal(eg_seen, c("1:2", "2:0"), info = "Grouped mutate() evaluates its expressions for empty groups too")
expect_equal(ungroup(eg_res), data.frame(eg_df, k = 1), info = "Results for empty groups add no rows to mutate()")

se_df <- data.frame(g1 = c("a", "a", "b", "b", "b"), g2 = c(1, 1, 2, 2, 2), x = 1:5)
expect_error(
  mutate(group_by(se_df, g1, g2), k = 1:2),
  paste0(
    "Problem while computing `k = 1:2`.\nx `k` must be size 3 or 1, not 2.\n",
    "i The error occurred in group 2: g1 = \"b\", g2 = 2."
  ),
  fixed = TRUE,
  info = "Grouped mutate() gives an error naming the group when a result has the wrong size"
)
expect_error(
  mutate(group_by(se_df, g1), 1:2),
  paste0(
    "Problem while computing `..1 = 1:2`.\nx `..1` must be size 3 or 1, not 2.\n",
    "i The error occurred in group 2: g1 = \"b\"."
  ),
  fixed = TRUE,
  info = "The size error of mutate() calls an unnamed expression by its position"
)
expect_error(
  mutate(se_df, k = 1:2),
  "Problem while computing `k = 1:2`.\nx `k` must be size 5 or 1, not 2.",
  fixed = TRUE,
  info = "Ungrouped mutate() gives an error when a result has the wrong size"
)
expect_error(
  mutate(data.frame(x = 1), y = 1:2),
  "Problem while computing `y = 1:2`.\nx `y` must be size 1, not 2.",
  fixed = TRUE,
  info = "The size error of mutate() on data with one row gives the size once"
)
expect_error(
  transmute(data.frame(x = 1), 1:2),
  "Problem while computing `..1 = 1:2`.\nx `..1` must be size 1, not 2.",
  fixed = TRUE,
  info = "The size error of transmute() on data with one row gives the size once"
)
expect_error(
  mutate(group_by(se_df, g1, g2), k = if (g1[1L] == "a") 1:3 else 1L),
  paste0(
    "Problem while computing `k = if (g1[1L] == \"a\") 1:3 else 1L`.\nx `k` must be size 2 or 1, not 3.\n",
    "i The error occurred in group 1: g1 = \"a\", g2 = 1."
  ),
  fixed = TRUE,
  info = "The size error of grouped mutate() gives the size of the group"
)
expect_error(
  transmute(group_by(data.frame(g = c(1, 2, 2), x = 1:3), g), y = 1:2),
  "Problem while computing `y = 1:2`.\nx `y` must be size 1, not 2.\ni The error occurred in group 1: g = 1.",
  fixed = TRUE,
  info = "The size error of grouped transmute() gives the size once for a group with one row"
)

nv_df <- data.frame(g = c("b", "a", "b", "c", "a"), x = 1:5)
expect_equal(
  ungroup(mutate(group_by(nv_df, g), k = nrow(filter(nv_df, x > 2)) + n())),
  data.frame(nv_df, k = c(5L, 5L, 5L, 4L, 5L)),
  info = "A verb can be used inside an expression of a grouped mutate()"
)
nv_fn <- function(data) {
  order <- 10
  vals <- 100
  mask <- 1000
  mutate(group_by(data, g), y = x + order + vals + mask)
}
expect_equal(
  nv_fn(nv_df)$y,
  1:5 + 1110,
  info = "Grouped mutate() finds local variables, even with the names of its own variables"
)
nv_rn <- data.frame(g = c("b", "a", "b"), x = c(1, 2, 3), row.names = c("r1", "r2", "r3"))
expect_equal(
  ungroup(mutate(group_by(nv_rn, g), m = mean(x))),
  data.frame(g = c("b", "a", "b"), x = c(1, 2, 3), m = c(2, 2, 2), row.names = c("r1", "r2", "r3")),
  info = "Grouped mutate() keeps the order and the names of the rows"
)

# A package can build on poorman with methods for its own class which call NextMethod()
mutate.poorman_ext <- function(.data, ...) { # nolint: object_name_linter.
  res <- NextMethod()
  attr(res, "ext") <- "kept"
  res
}
registerS3method("mutate", "poorman_ext", mutate.poorman_ext, envir = asNamespace("poorman"))
ext_df <- group_by(data.frame(g = c("b", "a", "b"), x = 1:3), g)
class(ext_df) <- c("poorman_ext", class(ext_df))
ext_fn <- function(data) {
  k <- 100
  mutate(data, y = x + k, n = n())
}
ext_res <- ext_fn(ext_df)
expect_equal(
  class(ext_res),
  c("poorman_ext", "grouped_df", "data.frame"),
  info = "Grouped mutate() keeps the class of an extension"
)
expect_equal(attr(ext_res, "ext"), "kept", info = "The method of an extension class is used by mutate()")
expect_equal(ext_res$y, c(101, 102, 103), info = "mutate() called through NextMethod() finds local variables")
expect_equal(ext_res$n, c(2L, 1L, 2L), info = "mutate() called through NextMethod() respects the groups")

# Each group has its own variables: an assignment in one group is not seen by the next (dplyr 1.0.10 leaks it)
df <- data.frame(g = c("a", "b", "a", "c", "b"), x = 1:5)
expect_equal(
  mutate(group_by(df, g), k = { if (!exists("cnt", inherits = FALSE)) cnt <- 0; cnt <- cnt + 1; cnt })$k, rep(1, 5),
  info = "grouped mutate() does not share assignments between groups"
)
expect_equal(
  mutate(group_by(df, g), a = { tmp <- 1; 1 }, b = exists("tmp", inherits = FALSE))$b, rep(TRUE, 5),
  info = "grouped mutate() shares assignments with the later expressions of the same group"
)
expect_equal(
  filter(group_by(df, g), { if (!exists("cnt", inherits = FALSE)) cnt <- 0; cnt <- cnt + 1; cnt < 2 })$x, 1:5,
  info = "grouped filter() does not share assignments between groups"
)
rm(df)
