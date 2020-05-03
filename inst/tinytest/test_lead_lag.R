expect_error(lag(1:5, n = -1), info = "Lag needs a non-negative integer")
expect_error(lead(1:5, n = -1), info = "Lead needs a non-negative integer")

expect_equal(
  lag(1:5, n = 2),
  c(NA, NA, 1, 2, 3),
  info = "Lag works for larger values of `n`"
)
expect_equal(
  lead(1:5, n = 2),
  c(3, 4, 5, NA, NA),
  info = "Lead works for larger values of `n`"
)

expect_equal(
  lag(1:5, default = 0),
  0:4,
  info = "Lag accepts non-NA defaults"
)
expect_equal(
  lead(1:5, default = 6),
  2:6,
  info = "Lead accepts non-NA defaults"
)

expect_error(
  lag(1:5, "NA"),
  info = "Lag performs _some_ type checking"
)
expect_error(
  lead(1:5, "NA"),
  info = "Lead performs _some_ type checking"
)

x <- factor(c("a", "b", "c"))
expect_equal(
  levels(lag(x)),
  c("a", "b", "c"),
  info = "Lag preserves factor levels"
)
expect_equal(
  levels(lead(x)),
  c("a", "b", "c"),
  info = "Lead preserves factor levels"
)
rm(x)

x <- as.Date("2013-01-01") + 1:3
y <- as.POSIXct(x)
expect_equal(class(lag(x)), "Date", info = "Lag preserves dates")
expect_equal(class(lead(x)), "Date", info = "Lead preserves dates")
expect_equal(class(lag(y)), c("POSIXct", "POSIXt"), info = "Lag preserves dates and times")
expect_equal(class(lead(y)), c("POSIXct", "POSIXt"), info = "Lead preserves dates and times")
rm(x, y)

data <- data.frame(
  name = c("Rob", "Pete", "Rob", "John", "Rob", "Pete", "John", "Pete", "John", "Pete", "Rob", "Rob"),
  time = c(3, 2, 5, 3, 2, 3, 2, 4, 1, 1, 4, 1),
  stringsAsFactors = FALSE
)
res <- data %>% group_by(name) %>% mutate(lag_time = lag(time))
expect_equal(
  res$lag_time[res$name == "Rob"],
  c(NA, head(data$time[data$name == "Rob"], -1)),
  info = "Lag works in grouped operations"
)
expect_equal(
  res$lag_time[res$name == "Pete"],
  c(NA, head(data$time[data$name == "Pete"], -1)),
  info = "Lag works in grouped operations"
)
expect_equal(
  res$lag_time[res$name == "John"],
  c(NA, head(data$time[data$name == "John"], -1)),
  info = "Lag works in grouped operations"
)
rm(data, res)

data <- data.frame(
  name = rep(c("Al", "Jen"), 3),
  score = rep(c(100, 80, 60), 2),
  stringsAsFactors = FALSE
)
res <- data %>% group_by(name) %>% mutate(next.score = lead(score))
expect_equal(
  res$next.score[res$name == "Al"],
  c(tail(data$score[data$name == "Al"], -1), NA),
  info = "Lead works in grouped operations"
)
expect_equal(
  res$next.score[res$name == "Jen"],
  c(tail(data$score[data$name == "Jen"], -1), NA),
  info = "Lead works in grouped operations"
)
rm(data, res)
