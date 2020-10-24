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
      split(res, list(res$am , res$cyl)),
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
expect_equal(out, data.frame(x = 1, y = data.frame(a = 1)), info = "named data frames are packed")

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

# Errors

df <- data.frame(x = 1:2, y = 1:2)
expect_error(df %>% mutate(y = NULL, a = sum(y)), info = "setting column to NULL makes it unavailable")
expect_error(
  df %>% group_by(x) %>% mutate(y = NULL, a = sum(y)),
  info = "setting column to NULL makes it unavailable for grouped df"
)
