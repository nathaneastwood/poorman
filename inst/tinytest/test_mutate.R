expect_equal(
  mtcars %>% mutate(mpg2 = mpg * 2),
  {
    res <- mtcars
    res[, "mpg2"] <- res$mpg * 2
    res
  },
  info = "Test mutate() can add a new column"
)

expect_equal(
  mtcars %>% mutate(mpg = NULL),
  {
    res <- mtcars
    res[, "mpg"] <- NULL
    res
  },
  info = "Test mutate() can remove columns"
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

expect_equal(
  mtcars %>% mutate(mpg2 = mpg * 2, mpg3 = mpg * 3),
  {
    res <- mtcars
    res[, "mpg2"] <- res$mpg * 2
    res[, "mpg3"] <- res$mpg * 3
    res
  },
  info = "Test mutate() can add multiple columns"
)

expect_equal(
  mtcars %>% mutate(mpg2 = mpg * 2, mpg4 = mpg2 * 2),
  {
    res <- mtcars
    res[, "mpg2"] <- res$mpg * 2
    res[, "mpg4"] <- res$mpg2 * 2
    res
  },
  info = "Test that newly created variables are available immediately with mutate()"
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
      split(res, list(res$am , res$cyl)),
      function(x) {
        x[, "col"] <- 1
        x
      }
    )))
    res[rownames(mtcars), ]
  },
  info = "mutate() works when there are missing groups"
)
