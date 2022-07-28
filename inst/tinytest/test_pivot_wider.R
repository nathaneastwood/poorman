library(tidyr)

# equivalence with tidyr::tidyr::pivot_wider() -----------------------------------------------

# Examples coming from: https://tidyr.tidyverse.org/articles/pivot.html#wider
# and from https://github.com/tidyverse/tidyr/blob/main/tests/testthat/test-pivot-wide.R


df <- tibble(key = c("x", "y", "z"), val = 1:3)
pv <- poorman::pivot_wider(df, names_from = "key", values_from = "val")

expect_equal(names(pv), c("x", "y", "z"))
expect_equal(nrow(pv), 1)

df <- tibble(a = 1, key = c("x", "y"), val = 1:2)
pv <- poorman::pivot_wider(df, names_from = "key", values_from = "val")

expect_equal(names(pv), c("a", "x", "y"))
expect_equal(nrow(pv), 1)


df <- tibble(a = 1:2, key = c("x", "y"), val = 1:2)
pv <- poorman::pivot_wider(df, names_from = "key", values_from = "val")

expect_equal(pv$a, c(1, 2))
expect_equal(pv$x, c(1, NA))
expect_equal(pv$y, c(NA, 2))


df <- tibble(
  a = c(1, 1),
  key = c("a", "b"),
  val = c(1, 2)
)

expect_error(
  poorman::pivot_wider(df, names_from = "key", values_from = "val"),
  pattern = "Some values of the columns specified"
)



### Examples from tidyr website

x <- fish_encounters %>%
  tidyr::pivot_wider(names_from = "station", values_from = "seen", values_fill = 0)

y <- fish_encounters %>%
  poorman::pivot_wider(
    names_from = "station",
    values_from = "seen",
    values_fill = 0
  )

expect_equal(x, y, ignore_attr = TRUE)


production <- expand_grid(
  product = c("A", "B"),
  country = c("AI", "EI"),
  year = 2000:2014
) %>%
  filter((product == "A" & country == "AI") | product == "B")

production$production <- rnorm(nrow(production))

x <- production %>%
  tidyr::pivot_wider(
    names_from = c(product, country),
    values_from = production
  )

y <- production %>%
  poorman::pivot_wider(
    names_from = c("product", "country"),
    values_from = "production"
  )

expect_identical(x, y)


x <- us_rent_income %>%
  tidyr::pivot_wider(
    names_from = variable,
    values_from = c(estimate, moe)
  )

y <- us_rent_income %>%
  poorman::pivot_wider(
    names_from = "variable",
    values_from = c("estimate", "moe")
  )

expect_identical(x, y)


x <- us_rent_income %>%
  tidyr::pivot_wider(
    names_from = variable,
    names_sep = ".",
    values_from = c(estimate, moe)
  )

y <- us_rent_income %>%
  poorman::pivot_wider(
    names_from = "variable",
    names_sep = ".",
    values_from = c("estimate", "moe")
  )

expect_identical(x, y)


contacts <- tribble(
  ~field,
  ~value,
  "name",
  "Jiena McLellan",
  "company",
  "Toyota",
  "name",
  "John Smith",
  "company",
  "google",
  "email",
  "john@google.com",
  "name",
  "Huxley Ratcliffe"
)
contacts$person_id <- cumsum(contacts$field == "name")

x <- contacts %>%
  tidyr::pivot_wider(names_from = field, values_from = value)

y <- contacts %>%
  poorman::pivot_wider(names_from = "field", values_from = "value")

expect_identical(x, y)



production <- expand_grid(
  product = c("A", "B"),
  country = c("AI", "EI"),
  year = 2000:2014
) %>%
  poorman::filter((product == "A" & country == "AI") | product == "B")

production$production <- rnorm(nrow(production))

x <- production %>%
  tidyr::pivot_wider(
    names_from = c(product, country),
    values_from = production,
    names_glue = "prod_{product}_{country}"
  )

y <- production %>%
  poorman::pivot_wider(
    names_from = c("product", "country"),
    values_from = "production",
    names_glue = "prod_{product}_{country}"
  )

expect_identical(x, y)


df <- data.frame(
  food = c("banana", "banana", "banana", "banana", "cheese", "cheese", "cheese", "cheese"),
  binary = c(rep(c("yes", "no"), 4)),
  car = c("toyota", "subaru", "mazda", "skoda", "toyota", "subaru", "mazda", "skoda"),
  fun = c(2, 4, 3, 6, 2, 4, 2, 3)
)

x <- df %>%
  tidyr::pivot_wider(
    id_cols = food,
    names_from = c(car, binary),
    names_glue = "{binary}_{car}",
    values_from = fun
  )

y <- df %>%
  poorman::pivot_wider(
    id_cols = "food",
    names_from = c("car", "binary"),
    names_glue = "{binary}_{car}",
    values_from = "fun"
  )

expect_equivalent(x, y) # ignore attributes
