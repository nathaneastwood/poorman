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
