x <- 1:3
expect_equal(
  case_when(
    x <= 1 ~ 1,
    x <= 2 ~ 2,
    x <= 3 ~ 3
  ),
  c(1, 2, 3),
  info = "case_when() matches values in order"
)

x <- 1:3
expect_equal(
  case_when(
    x <= 1 ~ 1,
    x <= 2 ~ 2
  ),
  c(1, 2, NA),
  info = "case_when() unmatched gets missing value"
)

x <- c(1:3, NA)
expect_equal(
  case_when(
    x <= 1 ~ 1,
    x <= 2 ~ 2,
    is.na(x) ~ 0
  ),
  c(1, 2, NA, 0),
  info = "case_when() missing values can be replaced"
)

expect_equal(
  case_when(
    c(TRUE, FALSE, NA) ~ 1:3,
    TRUE ~ 4L
  ),
  c(1L, 4L, 4L),
  info = "case_when() NA conditions"
)

expect_equal(
  case_when(
    TRUE ~ 1:3,
    FALSE ~ 4:6
  ),
  1:3,
  info = "case_when() atomic conditions 1"
)
expect_equal(
  case_when(
    NA ~ 1:3,
    TRUE ~ 4:6
  ),
  4:6,
  info = "case_when() atomic conditions 2"
)

expect_equal(
  case_when(
    TRUE ~ integer(),
    FALSE ~ integer()
  ),
  integer(),
  info = "case_when() zero-length conditions and values 1"
)
expect_equal(
  case_when(
    logical() ~ 1,
    logical() ~ 2
  ),
  numeric(),
  info = "case_when() zero-length conditions and values 2"
)

res <- data.frame(a = 1:3) %>%
  mutate(b = (function(x) case_when(x < 2 ~ TRUE, TRUE ~ FALSE))(a)) %>%
  pull()
expect_equal(res, c(TRUE, FALSE, FALSE), info = "case_when() can be used in anonymous functions")

res <- mtcars %>% mutate(efficient = case_when(mpg > 25 ~ TRUE, TRUE ~ FALSE)) %>% pull()
expect_equal(
  res,
  c(
    FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
    FALSE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE
  ),
  info = "case_when() can be used inside mutate()"
)

expect_error(
  case_when(c(TRUE, FALSE) ~ 1:3, c(FALSE, TRUE) ~ 1:2),
  info = "case_when() must return the correct amount of RHS values."
)

expect_error(
  case_when(50 ~ 1:3),
  info = "case_when() must return a logical vector."
)

expect_error(
  case_when(paste(50)),
  info = "case_when() requires formula inputs."
)

expect_error(
  case_when(),
  info = "No cases provided."
)
