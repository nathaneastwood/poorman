df <- data.frame(x = c(NA, "x.y", "x.z", "y.z"))

expect_equal(
  separate(df, x,  c("A", "B")),
  data.frame(
    A = c(NA, "x", "x", "y"),
    B = c(NA, "y", "z", "z")
  ),
  info = "separate() works with basic example"
)

expect_equal(
  separate(df, x,  c(NA, "B")),
  data.frame(
    B = c(NA, "y", "z", "z")
  ),
  info = "separate() correctly handles NA in 'into'"
)

expect_equal(
  separate(df, x,  c("A", "B"), remove = FALSE),
  data.frame(
    x = c(NA, "x.y", "x.z", "y.z"),
    A = c(NA, "x", "x", "y"),
    B = c(NA, "y", "z", "z")
  ),
  info = "separate() argument 'remove' works"
)

df <- data.frame(x = c("x", "x y", "x y z", NA))

expect_warning(
  separate(df, x, c("a", "b")),
  pattern = "Expected 2 pieces. Additional pieces discarded in 1 rows",
  info = "separate() warns if more pieces than cols in 'into' and extra = warn"
)

expect_silent(
  separate(df, x, c("a", "b"), extra = "drop"),
  info = "separate() is silent if more pieces than cols in 'into' and extra = drop"
)


expect_equal(
  df %>% separate(x, c("a", "b", "c")),
  data.frame(
    a = c("x", "x", "x", NA),
    b = c(NA, "y", "y", NA),
    c = c(NA, NA, "z", NA)
  )
)


df <- data.frame(x = c("x:1", "x:2", "y:4", "z", NA))

expect_equal(
  df %>% separate(x, c("key","value"), ":") %>% lapply(., class),
  list(key = "character", value = "character")
)
expect_equal(
  df %>% separate(x, c("key","value"), ":", convert = TRUE) %>% lapply(., class),
  list(key = "character", value = "integer"),
  info = "separate() argument 'convert' works"
)

df <- data.frame(x = c("x: 123", "y: error: 7"))
expect_equal(
  df %>% separate(x, c("key", "value"), ": ", extra = "merge"),
  data.frame(key = c("x", "y"), value = c("123", "error: 7")),
  info = "separate() works with extra = merge"
)

df <- data.frame(x = c(NA, "a1b", "c4d", "e9g"))
expect_equal(
  df %>% separate(x, c("A","B"), sep = "[0-9]"),
  data.frame(A = c(NA, "a", "c", "e"), B = c(NA, "b", "d", "g"))
)
