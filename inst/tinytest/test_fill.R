# all missings left unchanged 
df <- data.frame(
  lgl = c(NA, NA),
  int = c(NA_integer_, NA),
  dbl = c(NA_real_, NA),
  chr = c(NA_character_, NA)
)

down <- fill(df, lgl, int, dbl, chr)
up <- fill(df, lgl, int, dbl, chr, .direction = "up")

expect_identical(down, df)
expect_identical(up, df)


# filled down from last non-missing
df <- data.frame(x = c(NA, 1, NA, 2, NA, NA))

out <- fill(df, x)
expect_equal(out$x, c(NA, 1, 1, 2, 2, 2))

out <- fill(df, x, .direction = "up")
expect_equal(out$x, c(1, 1, 2, 2, NA, NA))

out <- fill(df, x, .direction = "downup")
expect_equal(out$x, c(1, 1, 1, 2, 2, 2))

out <- fill(df, x, .direction = "updown")
expect_equal(out$x, c(1, 1, 2, 2, 2, 2))


# missings filled up for each vector
df <- data.frame(
  lgl = c(NA, T),
  int = c(NA, 1L),
  dbl = c(NA, 1),
  chr = c(NA, "a")
)
out <- fill(df, everything(), .direction = "up")
expect_equal(out$lgl, c(TRUE, TRUE))
expect_equal(out$int, c(1L, 1L))
expect_equal(out$dbl, c(1, 1))
expect_equal(out$chr, c("a", "a"))


# NaN treated as missing
df <- data.frame(x = c(1, NaN, 2))

out <- fill(df, x)
expect_identical(out$x, c(1, 1, 2))

out <- fill(df, x, .direction = "up")
expect_identical(out$x, c(1, 2, 2))

# respect grouping
df <- data.frame(x = c(1, 1, 2), y = c(1, NA, NA))
out <- df %>%
  group_by(x) %>%
  fill(y)
expect_equal(out$y, c(1, 1, NA))

# Grouped fill() fills within each group and keeps the rows in their order
df <- data.frame(g = c(2, 1, 2, 1, 2, 1), x = c(NA, 1, 3, NA, NA, NA), y = c("p", NA, NA, "s", NA, NA))
gf <- group_by(df, g)
expect_silent(res <- fill(gf, x, y), info = "grouped fill() does not add the grouping variables with a message")
expect_equal(res$x, c(NA, 1, 3, 1, 3, 1), info = "grouped fill() down")
expect_equal(res$y, c("p", NA, "p", "s", "p", "s"), info = "grouped fill() down with a character column")
expect_equal(group_data(res)$.rows, list(c(2L, 4L, 6L), c(1L, 3L, 5L)), info = "grouped fill() keeps the groups")
expect_equal(fill(gf, x, .direction = "up")$x, c(3, 1, 3, NA, NA, NA), info = "grouped fill() up")
res <- fill(gf, x, y, .direction = "downup")
expect_equal(res$x, c(3, 1, 3, 1, 3, 1), info = "grouped fill() downup")
expect_equal(res$y, c("p", "s", "p", "s", "p", "s"), info = "grouped fill() downup with a character column")
expect_equal(fill(gf, y, .direction = "updown")$y, c("p", "s", "p", "s", "p", "s"), info = "grouped fill() updown")
f <- function() {
  cols <- "x"
  fill(gf, all_of(cols))
}
expect_equal(f()$x, c(NA, 1, 3, 1, 3, 1), info = "grouped fill() finds local variables of the caller")
expect_equal(
  fill(group_by(data.frame(g = c(NA, 1, NA), x = c(1, NA, NA)), g), x)$x, c(1, NA, 1),
  info = "grouped fill() treats a missing group as a group"
)
expect_equal(
  fill(group_by(data.frame(g = c(1, 2, 1), f = factor(c("a", NA, NA))), g), f)$f, factor(c("a", NA, "a")),
  info = "grouped fill() with a factor column"
)

# NULLs are replaced in list columns
df <- data.frame(g = c(1, 1, 2))
df$l <- list(1, NULL, 3)
expect_equal(fill(df, l)$l, list(1, 1, 3), info = "fill() replaces NULLs in a list column")
expect_equal(fill(df, l, .direction = "up")$l, list(1, 3, 3), info = "fill() up replaces NULLs in a list column")
expect_equal(fill(group_by(df, g), l)$l, list(1, 1, 3), info = "grouped fill() replaces NULLs in a list column")
rm(df, gf, res, f)
