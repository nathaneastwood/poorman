expect_equal(
  recode(1:2, "a", "b"),
  c("a", "b"),
  info = "positional substitution works"
)

expect_equal(
  recode(1:2, `2` = "b", `1` = "a"),
  c("a", "b"),
  "names override positions"
)


x1 <- letters[1:3]
expect_equal(
  recode(x1, a = "apple", .default = NA_character_),
  c("apple", NA, NA),
  info = "named substitution works for character"
)
x2 <- factor(x1)
expect_equal(
  recode(x2, a = "apple", .default = NA_character_),
  factor(c("apple", NA, NA)),
  info = "named substitution works for factor"
)

expect_equal(
  recode(c(1, NA), "a"),
  c("a", NA),
  info = "missing values replaced by missing argument"
)
expect_equal(
  recode(c(1, NA), "a", .missing = "b"),
  c("a", "b"),
  info = "missing values replaced by missing argument"
)
expect_equal(
  recode(c(letters[1:3], NA), .missing = "A"),
  c("a", "b", "c", "A"),
  info = "missing values replaced by missing argument"
)

expect_warning(
  expect_equal(recode(c(1, 2), "a"), c("a", NA))
)
expect_equal(
  recode(c(1, 2), "a", .default = "b"),
  c("a", "b"),
  info = "unmatched value replaced by default argument"
)
expect_equal(
  recode(letters[1:3], .default = "A"),
  c("A", "A", "A"),
  info = "unmatched value replaced by default argument"
)

expect_equal(
  recode(c(1, 2, NA), "a", .default = "b", .missing = "c"),
  c("a", "b", "c"),
  info = "missing and default place nicely together"
)

expect_equal(recode("x", x = "a"), "a", info = "can give name x")

x <- rep(1:3, 3)
expect_equal(
  recode(x, `3` = 10L, .default = x),
  rep(c(1L, 2L, 10L), 3),
  info = ".default works when not all values are named"
)

x <- letters[1:3]
expect_equal(recode(x, a = "A"), c("A", "b", "c"), info = ".default is aliased to .x when missing and compatible")
n <- 1:3
expect_equal(recode(n, `1` = 10L), c(10L, 2L, 3L), info = ".default is aliased to .x when missing and compatible")

x <- letters[1:3]
expect_warning(
  expect_equal(recode(x, a = 1), c(1L, NA, NA)),
  info = ".default is not aliased to .x when missing and not compatible"
)
n <- 1:3
expect_warning(
  expect_equal(recode(n, `1` = "a"), c("a", NA, NA)),
  info = ".default is not aliased to .x when missing and not compatible"
)

expect_warning(
  recode(1:3, `1` = "a"),
  info = "treated as NA"
)
expect_warning(
  recode_factor(letters[1:3], b = 1, c = 2),
  info = "conversion of unreplaced values to NA gives warning"
)

# factor ------------------------------------------------------------------

expect_equal(
  recode(factor(letters[1:3]), a = "A"),
  factor(c("A", "b", "c")),
  info = "default .default works with factors"
)

f <- factor(letters[1:3])
expect_equal(recode(f, a = 1, b = 2, c = 3), c(1, 2, 3), info = "can recode factor to double")
expect_equal(recode(f, a = 1, b = 2), c(1, 2, NA), info = "can recode factor to double")
expect_equal(recode(f, a = 1, b = 2, .default = 99), c(1, 2, 99), info = "can recode factor to double")

x <- c(1:3, NA)
expect_warning(
  expect_equal(
    recode_factor(x, `1` = "z", `2` = "y"),
    factor(c("z", "y", NA, NA), levels = c("z", "y"))
  ),
  info = "recode_factor() handles .missing and .default levels"
)
expect_equal(
  recode_factor(x, `1` = "z", `2` = "y", .default = "D"),
  factor(c("z", "y", "D", NA), levels = c("z", "y", "D")),
  info = "recode_factor() handles .missing and .default levels"
)
expect_equal(
  recode_factor(x, `1` = "z", `2` = "y", .default = "D", .missing = "M"),
  factor(c("z", "y", "D", "M"), c("z", "y", "D", "M")),
  info = "recode_factor() handles .missing and .default levels"
)

expected <- factor(c("a", "z", "y"), levels = c("z", "y", "a"))
x1 <- letters[1:3]
x2 <- factor(x1)
expect_equal(recode_factor(x1, b = "z", c = "y"), expected, info = "recode_factor() handles vector .default")
expect_equal(recode_factor(x2, b = "z", c = "y"), expected, info = "recode_factor() handles vector .default")
expect_equal(
  recode_factor(x1, b = "z", c = "y", .default = x1),
  expected,
  info = "recode_factor() handles vector .default"
)
expect_equal(
  recode_factor(x2, b = "z", c = "y", .default = x1),
  expected,
  info = "recode_factor() handles vector .default"
)

expect_equal(
  recode(factor(letters[1:4]), d = "c", b = "a"),
  factor(c("a", "a", "c", "c"), levels = c("a", "c")),
  info = "can recode factor with redundant levels"
)
expect_equal(
  recode_factor(letters[1:4], d = "c", b = "a"),
  factor(c("a", "a", "c", "c"), levels = c("c", "a")),
  info = "can recode factor with redundant levels"
)

# Errors --------------------------------------------
expect_error(recode(factor("a"), a = 5, .missing = 10), info = ".missing is not supported for factors")
expect_error(recode("a", b = 5, "c"), info = "All arguments must be named")
expect_error(recode(factor("a"), b = 5, "c"), info = "All arguments must be named")
expect_error(recode(1:5), info = "No replacements provided")
expect_error(recode("a"), info = "No replacements provided")
expect_error(recode(factor("a")), info = "No replacements provided")
