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
  list(model = "lm", model = "lm", model = "lm"),
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
