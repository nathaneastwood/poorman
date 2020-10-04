expect_equal(
  unite(data.frame(x = "a", y = "b", stringsAsFactors = FALSE), z, x:y),
  data.frame(z = "a_b", stringsAsFactors = FALSE),
  info = "unite() pastes columns together and removes old columns"
)

expect_equal(
  unite(data.frame(x = "a", y = "b", stringsAsFactors = FALSE), x, x:y),
  data.frame(x = "a_b", stringsAsFactors = FALSE),
  info = "unite() does not remove new col in case of name clash"
)

df <- data.frame(g = 1, x = "a", stringsAsFactors = FALSE) %>% group_by(g)
rs <- df %>% unite(x, x)
expect_equal(df, rs, info = "unite() preserves grouping")
expect_equal(class(df), class(rs), info = "unite() preserves grouping")
expect_equal(group_vars(df), group_vars(rs), info = "unite() preserves grouping")

df <- data.frame(g = 1, x = "a", stringsAsFactors = FALSE) %>% group_by(g)
rs <- df %>% unite(gx, g, x)
expect_equal(rs$gx, "1_a", info = "unite() drops grouping when needed")
expect_equal(group_vars(rs), character(), info = "unite() drops grouping when needed")

expect_equal(
  unite(data.frame(x = "a", y = "b", stringsAsFactors = FALSE), "z"),
  data.frame(z = "a_b", stringsAsFactors = FALSE),
  info = "Passing no columns uses all columns"
)

df <- data.frame(x = c("a", "a", NA, NA), y = c("b", NA, "b", NA), stringsAsFactors = FALSE)
out <- unite(df, "z", x:y, na.rm = TRUE)
expect_equal(out$z, c("a_b", "a", "b", ""), info = "unite() can remove missing vars on request")

df <- data.frame(
  x = c("x", "y", "z"),
  lgl = NA,
  dbl = NA_real_,
  chr = NA_character_,
  stringsAsFactors = FALSE
)
expect_equal(
  unite(df, "out", any_of(c("x", "lgl")), na.rm = TRUE)$out,
  c("x", "y", "z"),
  info = "unite() can remove missing vars on request even for logical NAs"
)
expect_equal(
  unite(df, "out", any_of(c("x", "dbl")), na.rm = TRUE)$out,
  c("x", "y", "z"),
  info = "unite() can remove missing vars on request even for numeric NAs"
)
expect_equal(
  unite(df, "out", any_of(c("x", "chr")), na.rm = TRUE)$out,
  c("x", "y", "z"),
  info = "unite() can remove missing vars on request even for character NAs"
)
