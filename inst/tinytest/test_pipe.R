expect_equal(
  iris %>% head,
  head(iris),
  info = "Function calls without brackets works."
)

expect_equal(
  iris %>% head(10),
  head(iris, 10),
  info = "Use with lhs as first argument."
)

expect_equal(
  "Ceci n'est pas une pipe" %>% gsub("une", "un", .),
  "un",
  info = "Using the dot place-holder."
)

expect_equal(
  1:10 %>% paste0(LETTERS[.]),
  paste0(1:10, LETTERS[1:10]),
  info = "When dot is nested, lhs is still placed first."
)

expect_equal(
  iris %>%
    {
      rbind(head(., n = 1), tail(., n = 1))
    },
  rbind(head(iris, n = 1), tail(iris, n = 1)),
  info = "Lambda expressions"
)

expect_equal(
  iris %>%
    {
      my_data <- .
      size <- 1
      rbind(head(my_data, size), tail(my_data, size))
    },
  rbind(head(iris, n = 1), tail(iris, n = 1)),
  info = "Renaming in lambda expressions"
)
