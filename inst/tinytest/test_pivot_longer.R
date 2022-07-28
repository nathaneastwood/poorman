library(tidyr)

# Equivalence with tidyr::tidyr::pivot_longer -------------------------------------------

# Examples coming from: https://tidyr.tidyverse.org/articles/pivot.html#longer

x <- relig_income %>%
  tidyr::pivot_longer(!religion, names_to = "income", values_to = "count")

y <- relig_income %>%
  poorman::pivot_longer(cols = -religion, names_to = "income", values_to = "count")

expect_equal(x, y, ignore_attr = TRUE)



x <- billboard %>%
  tidyr::pivot_longer(
    cols = starts_with("wk"),
    names_to = "week",
    values_to = "rank"
  )

y <- billboard %>%
  poorman::pivot_longer(
    cols = starts_with("wk"),
    names_to = "week",
    values_to = "rank"
  )

expect_equal(x, y, ignore_attr = TRUE)


x <- billboard %>%
  tidyr::pivot_longer(
    cols = starts_with("wk"),
    names_to = "week",
    values_to = "rank",
    values_drop_na = TRUE
  )

y <- billboard %>%
  poorman::pivot_longer(
    cols = starts_with("wk"),
    names_to = "week",
    values_to = "rank",
    values_drop_na = TRUE
  )

expect_equal(x, y, ignore_attr = TRUE)



x <- billboard %>%
  tidyr::pivot_longer(
    cols = starts_with("wk"),
    names_to = "week",
    names_prefix = "wk",
    values_to = "rank",
    values_drop_na = TRUE
  )

y <- billboard %>%
  poorman::pivot_longer(
    cols = starts_with("wk"),
    names_to = "week",
    names_prefix = "wk",
    values_to = "rank",
    values_drop_na = TRUE
  )

expect_equal(x, y, ignore_attr = TRUE)



suppressWarnings({
  x <- who %>%
    tidyr::pivot_longer(
      cols = 5:60,
      names_to = c("diagnosis", "gender", "age"),
      names_sep = "_",
      values_to = "count"
    )
})

y <- who %>%
  poorman::pivot_longer(
    cols = 5:60,
    names_to = c("diagnosis", "gender", "age"),
    names_sep = "_",
    values_to = "count"
  )

expect_equal(x, y, ignore_attr = TRUE)


x <- who %>%
  tidyr::pivot_longer(
    cols = new_sp_m014:newrel_f65,
    names_to = c("diagnosis", "gender", "age"),
    names_pattern = "new_?(.*)_(.)(.*)",
    values_to = "count"
  )

y <- who %>%
  poorman::pivot_longer(
    cols = 5:60,
    names_to = c("diagnosis", "gender", "age"),
    names_pattern = "new_?(.*)_(.)(.*)",
    values_to = "count"
  )

expect_equal(x, y, ignore_attr = TRUE)




# tests coming from tidyr's repo
# https://github.com/tidyverse/tidyr/blob/main/tests/testthat/test-pivot-long.R

df <- tibble(x = 1:2, y = 3:4)
pv <- poorman::pivot_longer(df, x:y)

expect_equal(names(pv), c("name", "value"))
expect_equal(pv$name, rep(names(df), 2))
expect_equal(pv$value, c(1, 3, 2, 4))


df <- tibble(
  x = c(1, 2),
  y = c(10, 20),
  z = c(100, 200),
)
pv <- poorman::pivot_longer(df, 1:3)

expect_equal(pv$value, c(1, 10, 100, 2, 20, 200))


df <- tibble(x = 1:2, y = 2, z = 1:2)
pv <- poorman::pivot_longer(df, y:z)

expect_equal(names(pv), c("x", "name", "value"))
expect_equal(pv$x, rep(df$x, each = 2))


df <- data.frame(x = c(1, NA), y = c(NA, 2))
pv <- poorman::pivot_longer(df, x:y, values_drop_na = TRUE)

expect_equal(pv$name, c("x", "y"))
expect_equal(pv$value, c(1, 2))


df <- data.frame(x = factor("a"), y = factor("b"))
pv <- poorman::pivot_longer(df, x:y)

expect_equal(pv$value, factor(c("a", "b")))


df <- tibble(x = 1, y = 2)

expect_error(
  poorman::pivot_longer(df, y, names_to = "x"),
  pattern = "Some values of the columns specified in 'names_to' are already present"
)
